{-# LANGUAGE OverloadedStrings #-}

module Person (
  initPerson
  , runPerson
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

newtype KeepConnectionCommand = KeepConnectionCommand Bool
data DisconnectEvent = DisconnectEvent
data ConsoleCommand = UserInput Text

initPerson :: IO (Output Text, Output ByteString -> IO ())
initPerson = do
    personBox <- spawn unbounded
    return (fst personBox, runPerson $ snd personBox)

runPerson :: Input Text -> Output ByteString -> IO ()
runPerson input output = do 
    network <- compile $ keepConnectedTask input output    
    actuate network

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

sendToPersonConsumer :: Handler ConsoleCommand -> Consumer Text IO ()
sendToPersonConsumer sendAction = do
  text <- await
  liftIO $ sendAction $ UserInput text
  sendToPersonConsumer sendAction

keepConnectedTask :: Input Text -> Output ByteString -> MomentIO ()
keepConnectedTask fromConsoles toConsoles = do
    (consoleCommandEvent, fireConsoleCommandEvent) <- newEvent
    liftIOLater $ do async $ runEffect $ fromInput fromConsoles >-> sendToPersonConsumer fireConsoleCommandEvent
                     return ()
    (disconnectionEvent, fireDisconnection) <- newEvent
    (bSocket, fireUpdateSocket) <- newBehavior Nothing
    let keepConnectionEvent = keepConnectionCommandToEvent <$> filterE isKeepConnectionCommand consoleCommandEvent
    bKeepConnection <- stepper False keepConnectionEvent

    (serverEvent, fireServerEvent) <- newEvent

    reactimate $ connectToServer fireDisconnection fireUpdateSocket toConsoles fireServerEvent <$ whenE (isNothing <$> bSocket) (filterE id keepConnectionEvent)
    reactimate $ connectToServer fireDisconnection fireUpdateSocket toConsoles fireServerEvent <$ whenE bKeepConnection disconnectionEvent
    reactimate $ sendCommand toConsoles <$> bSocket <@> ((\(UserInput txt) -> txt) <$> filterE notCommandInput consoleCommandEvent)

    keepLoggedTask toConsoles bSocket serverEvent

keepLoggedTask :: Output ByteString -> Behavior (Maybe Socket) -> Event ServerEvent -> MomentIO ()
keepLoggedTask toConsoles bSocket serverEvent = do
    reactimate $ sendCommand toConsoles <$> bSocket <@> ("5" <$ filterE (== CodepagePrompt) serverEvent)
    reactimate $ loadLoginAndSendCommand <$> bSocket <@ (filterE (== LoginPrompt) serverEvent)
    reactimate $ loadPasswordAndSendCommand <$> bSocket <@ (filterE (== PasswordPrompt) serverEvent)
    reactimate $ sendCommand toConsoles <$> bSocket <@> ("" <$ filterE (== WelcomePrompt) serverEvent)
    where loadLoginAndSendCommand socket = do conf <- DC.load [Required personCfgFileName]
                                              mbLogin <- DC.lookup conf "person.login"
                                              handle mbLogin
                                              where handle (Just login) = sendCommand toConsoles socket login
                                                    handle Nothing = DTIO.putStrLn "failed to load person login"
          loadPasswordAndSendCommand socket = do conf <- DC.load [Required personCfgFileName]
                                                 mbPassword <- DC.lookup conf "person.password"
                                                 handle mbPassword
                                                 where handle (Just pass) = sendCommand toConsoles socket pass
                                                       handle Nothing = DTIO.putStrLn "failed to load person password"

personCfgFileName :: String
personCfgFileName = "person.cfg"

sendCommand :: Output ByteString -> Maybe Socket -> Text -> IO ()
sendCommand _ (Just sock) txt = NST.send sock $ encodeUtf8 $ snoc txt '\n'
sendCommand consoleBox Nothing _ = do atomically $ PC.send consoleBox $ encodeUtf8 "no connection to server"
                                      return ()

keepConnectionCommandToEvent :: ConsoleCommand -> Bool
keepConnectionCommandToEvent (UserInput "/conn") = True
keepConnectionCommandToEvent (UserInput "/unconn") = False

isKeepConnectionCommand :: ConsoleCommand -> Bool
isKeepConnectionCommand (UserInput "/conn") = True
isKeepConnectionCommand (UserInput "/unconn") = True
isKeepConnectionCommand _ = False

notCommandInput :: ConsoleCommand -> Bool
notCommandInput (UserInput input) = not $ isPrefixOf "/" input

connectToServer :: Handler DisconnectEvent -> Handler (Maybe Socket) -> Output ByteString -> Handler ServerEvent -> IO ()
connectToServer fireDisconnection updateSocketBehavior toConsoles fireServerEvent = do
    (sock, addr) <- NST.connectSock "bylins.su" "4000"
    updateSocketBehavior $ Just sock
    logFile <- openFile "log" WriteMode
    (persToLogOut, persToLogIn, sealPersToLog) <- spawn' unbounded
    (parseServerTextOut, parseServerTextIn, sealParseServerText) <- spawn' unbounded
    let closeSockOnEof = NST.closeSock sock
    let closeLogFile = liftIO $ hClose logFile
    let fireDisconnectionEvent = liftIO $ fireDisconnection DisconnectEvent
    let seal f = liftIO $ atomically f
    let cleanup = closeLogFile >> liftIO (updateSocketBehavior Nothing) >> fireDisconnectionEvent 
    async $ do runEffect $ (fromSocket sock (2^15) >> closeSockOnEof) >-> toOutput (toConsoles <> parseServerTextOut <> persToLogOut) >> cleanup
               performGC 
    async $ do runEffect $ fromInput persToLogIn >-> PBS.toHandle logFile >> seal sealPersToLog
               performGC
    async $ do runEffect $ parseProducer (fromInput parseServerTextIn) >-> fireServerEventConsumer fireServerEvent >> seal sealParseServerText
               performGC
    return ()
                              
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
