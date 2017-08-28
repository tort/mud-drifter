{-# LANGUAGE OverloadedStrings #-}

module Person (
  runPerson
  , parseProducer -- TODO move to separate module
) where

import Control.Applicative ((<|>))
import Data.Monoid
import Data.Char
import Data.Text as DT
import qualified Data.Text.IO as DTIO
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Maybe
import qualified Network.Simple.TCP as NST
import qualified Network.Socket.ByteString as NBS
import qualified Network.Socket as NS
import Pipes
import Pipes.Concurrent
import qualified Pipes.Concurrent as PC
import qualified Pipes.ByteString as PBS
import Pipes.Network.TCP as PNT
import System.IO as SIO (hClose, openFile, Handle, withFile, IOMode(WriteMode))
import Data.Text.Encoding
import Data.ByteString.Char8 as DBC8 hiding (isInfixOf, isPrefixOf, snoc, putStrLn)
import Control.Concurrent.Async
import Parser
import Pipes.Attoparsec
import Pipes.Parse
import qualified Data.Configurator as DC
import Data.Configurator.Types

newtype GoDirectionAction = GoDirectionAction Text
data MoveRequest = MoveRequest TaskKey LocData
type TaskKey = Int
type SendToConsolesAction = ByteString -> IO ()

newtype KeepConnectionCommand = KeepConnectionCommand Bool
data DisconnectEvent = DisconnectEvent
data ConsoleCommand = UserInput Text | KeepConn Bool
data ServerCommand = ServerCommand Text

runPerson :: Output ByteString -> IO (Output Text)
runPerson output = do
    personBox <- spawn $ bounded 1024
    network <- compile $ personTask (snd personBox) output
    actuate network
    return $ fst personBox

personTask :: Input Text -> Output ByteString -> MomentIO ()
personTask fromConsoles toConsoles = do
    keepLoggedTask fromConsoles toConsoles

sendServerCommandTask :: Behavior (Maybe Socket) -> SendToConsolesAction -> MomentIO (Handler ServerCommand)
sendServerCommandTask bSocket sendToConsolesAction = do
  (serverCommandEvent, fireServerCommand) <- newEvent
  let txt = (\(ServerCommand txt) -> txt) <$> serverCommandEvent
  reactimate $ sendCommand sendToConsolesAction <$> bSocket <@> txt
  return fireServerCommand

writeLog :: IO (Output ByteString, STM ())
writeLog = do
  logFile <- openFile "log" WriteMode
  (persToLogOut, input, seal) <- spawn' $ bounded 1024
  async $ do runEffect $ fromInput input >-> PBS.toHandle logFile >> (liftIO $ hClose logFile) >> (liftIO $ DBC8.putStr "write log channel closed\n")
             performGC
  return (persToLogOut, seal)

fireEventsFromConsolesInput :: SendToConsolesAction -> Input Text -> MomentIO (Event ConsoleCommand)
fireEventsFromConsolesInput sendToConsolesAction fromConsolesChan = do
    (consoleCommandEvent, trigger) <- newEvent
    liftIOLater $ do async $ runEffect $ fromInput fromConsolesChan >-> handleInputFromConsolesConsumer sendToConsolesAction trigger
                     return ()
    return consoleCommandEvent

moveToTask :: Event MoveRequest -> Event ServerEvent -> MomentIO ()
moveToTask moveRequest serverEvent = do
    let locEvent = filterE isLocation serverEvent
    let moveEvent = filterE isMove serverEvent
    bRequests <- accumB [] $ addRequestEvent moveRequest
    let changeCurrLocEvent = (unions [(\l@(Location locData) -> const locData) <$> locEvent
                                     ,(\m@(Move _ locData) -> const locData) <$> moveEvent])

    reactimate $ moveCommand <$> bRequests <@> changeCurrLocEvent

addRequestEvent :: Event MoveRequest -> Event ([(TaskKey, LocData)] -> [(TaskKey, LocData)])
addRequestEvent moveRequest = (\mr@(MoveRequest k dest) acc -> (k, dest) : acc) <$> moveRequest

moveCommand :: [(TaskKey, LocData)] -> (LocData -> LocData) -> IO ()
moveCommand = undefined

isLocation :: ServerEvent -> Bool
isLocation (Location _) = True
isLocation _ = False

isMove :: ServerEvent -> Bool
isMove (Move _ _) = True
isMove _ = False

handleInputFromConsolesConsumer :: SendToConsolesAction -> Handler ConsoleCommand -> Consumer Text IO ()
handleInputFromConsolesConsumer sendToConsolesAction fireConsoleCommandEvent = do
  text <- await
  liftIO $ handleInputFromConsoles sendToConsolesAction fireConsoleCommandEvent text
  handleInputFromConsolesConsumer sendToConsolesAction fireConsoleCommandEvent

handleInputFromConsoles :: SendToConsolesAction -> Handler ConsoleCommand -> Text -> IO ()
handleInputFromConsoles sendToConsoleAction consoleCommandEventTrigger "/conn" = consoleCommandEventTrigger $ KeepConn True
handleInputFromConsoles sendToConsoleAction consoleCommandEventTrigger "/unconn" = consoleCommandEventTrigger $ KeepConn False
handleInputFromConsoles sendToConsoleAction consoleCommandEventTrigger txt
  | isPrefixOf "/" txt = sendToConsoleAction $ encodeUtf8 $ "unknown command " <> txt
  | otherwise = consoleCommandEventTrigger $ UserInput txt

keepConnectedTask :: Input Text -> Output ByteString -> Handler ServerEvent -> MomentIO (Handler ServerCommand)
keepConnectedTask fromConsoles toConsoles fireServerEvent = do
    let sendToConsolesAction = sendToConsoles toConsoles
    consoleCommandEvent <- fireEventsFromConsolesInput sendToConsolesAction fromConsoles
    (disconnectionEvent, fireDisconnection) <- newEvent
    let keepConnectionEvent = keepConnectionCommandToEvent <$> (filterE isKeepConnectionCommand consoleCommandEvent)
    bKeepConnection <- stepper False keepConnectionEvent
    (bSocket, fireUpdateSocket) <- newBehavior Nothing
    sendServerCommand <- sendServerCommandTask bSocket sendToConsolesAction

    reactimate $ connectToServer fireDisconnection fireUpdateSocket toConsoles fireServerEvent <$ whenE (isNothing <$> bSocket) (filterE id keepConnectionEvent)
    reactimate $ connectToServer fireDisconnection fireUpdateSocket toConsoles fireServerEvent <$ whenE bKeepConnection disconnectionEvent
    reactimate $ sendServerCommand <$> ((\(UserInput txt) -> ServerCommand txt) <$> filterE notCommandInput consoleCommandEvent)
    return sendServerCommand

keepLoggedTask :: Input Text -> Output ByteString -> MomentIO ()
keepLoggedTask fromConsoles toConsoles = do
    (serverEvent, fireServerEvent) <- newEvent
    sendServerCommand <- keepConnectedTask fromConsoles toConsoles fireServerEvent

    reactimate $ sendServerCommand <$> ServerCommand <$> ("5" <$ filterE (== CodepagePrompt) serverEvent)
    reactimate $ loadLoginAndSendCommand sendServerCommand <$ (filterE (== LoginPrompt) serverEvent)
    reactimate $ loadPasswordAndSendCommand sendServerCommand <$ (filterE (== PasswordPrompt) serverEvent)
    reactimate $ sendServerCommand <$> ServerCommand <$> ("" <$ filterE (== WelcomePrompt) serverEvent)
      where loadLoginAndSendCommand sendServerCommand = do conf <- DC.load [Required personCfgFileName]
                                                           mbLogin <- DC.lookup conf "person.login"
                                                           handle mbLogin
                                                             where handle (Just login) = sendServerCommand $ ServerCommand login
                                                                   handle Nothing = DTIO.putStrLn "failed to load person login"
            loadPasswordAndSendCommand sendServerCommand = do conf <- DC.load [Required personCfgFileName]
                                                              mbPassword <- DC.lookup conf "person.password"
                                                              handle mbPassword
                                                                where handle (Just pass) = sendServerCommand $ ServerCommand pass
                                                                      handle Nothing = DTIO.putStrLn "failed to load person password"

personCfgFileName :: String
personCfgFileName = "person.cfg"

sendCommand :: SendToConsolesAction -> Maybe Socket -> Text -> IO ()
sendCommand _ (Just sock) txt = NST.send sock $ encodeUtf8 $ snoc txt '\n'
sendCommand sendToConsolesAction Nothing _ = sendToConsolesAction "no connection to server"

sendToConsoles :: Output ByteString -> SendToConsolesAction
sendToConsoles channel msg = do atomically $ PC.send channel $ msg <> "\n"
                                return ()

keepConnectionCommandToEvent :: ConsoleCommand -> Bool
keepConnectionCommandToEvent (KeepConn v) = v

isKeepConnectionCommand :: ConsoleCommand -> Bool
isKeepConnectionCommand (KeepConn _) = True
isKeepConnectionCommand _ = False

notCommandInput :: ConsoleCommand -> Bool
notCommandInput (UserInput _) = True
notCommandInput _ = False

connectToServer :: Handler DisconnectEvent -> Handler (Maybe Socket) -> Output ByteString -> Handler ServerEvent -> IO ()
connectToServer fireDisconnection updateSocketBehavior toConsoles fireServerEvent = do
    (sock, addr) <- NST.connectSock "bylins.su" "4000"
    updateSocketBehavior $ Just sock

    let closeSockOnEof = NST.closeSock sock
    let fireDisconnectionEvent = liftIO $ fireDisconnection DisconnectEvent

    (writeLogChan, sealLogChain) <- writeLog
    (parseServerInputChan, sealServerInputParser) <- fireEventsFromServerInput fireServerEvent
    let cleanup = liftIO (updateSocketBehavior Nothing) >> (liftIO $ atomically sealLogChain) >> (liftIO $ atomically sealServerInputParser)
    async $ do runEffect $ (fromSocket sock (2^15) >> (liftIO $ DBC8.putStr "socket channel finished\n") >> closeSockOnEof >> cleanup ) >-> toOutput (toConsoles <> parseServerInputChan <> writeLogChan) >> fireDisconnectionEvent
               performGC
    return ()

fireEventsFromServerInput :: Handler ServerEvent -> IO (Output ByteString, STM ())
fireEventsFromServerInput fireServerEvent = do
    (parseServerTextOut, parseServerTextIn, seal) <- spawn' $ bounded 1024
    async $ do runEffect $ parseProducer (fromInput parseServerTextIn) >-> fireServerEventConsumer fireServerEvent
               performGC
    return (parseServerTextOut, seal)

parseProducer :: Producer ByteString IO () -> Producer (Maybe (Either ParsingError ServerEvent)) IO ()
parseProducer src = do
    (result, partial) <- liftIO $ runStateT (parse serverInputParser) src
    continue result partial
    where continue result@(Just (Right _)) partial = do yield result
                                                        parseProducer partial
          continue (Just (Left err)) _ = do liftIO $ DBC8.putStr "\nerror"
          continue Nothing _ = do liftIO $ DBC8.putStr "\nparsed entire stream"

fireServerEventConsumer :: Handler ServerEvent -> Consumer (Maybe (Either ParsingError ServerEvent)) IO ()
fireServerEventConsumer fireServerEvent = do
    event <- await
    liftIO $ handleEvent event
    fireServerEventConsumer fireServerEvent
    where handleEvent Nothing = return ()
          handleEvent (Just (Left err)) = return ()
          handleEvent (Just (Right event)) = fireServerEvent event

sendToConsoleConsumer :: (ByteString -> IO ()) -> Consumer ByteString IO ()
sendToConsoleConsumer action = do
    text <- await
    liftIO $ action text
    sendToConsoleConsumer action

printDirection :: GoDirectionAction -> IO ()
printDirection (GoDirectionAction direction) = DTIO.putStrLn direction

removePathHead :: Maybe [Text] -> Maybe [Text]
removePathHead Nothing = Nothing
removePathHead (Just []) = Nothing
removePathHead x@(Just xs) = fmap Prelude.tail x

bPathNotEmpty :: Behavior (Maybe [Text]) -> Behavior Bool
bPathNotEmpty = fmap pathNotEmpty

pathNotEmpty :: Maybe [Text] -> Bool
pathNotEmpty Nothing = False
pathNotEmpty (Just []) = False
pathNotEmpty (Just xs) = True
