{-# LANGUAGE OverloadedStrings #-}

module Person (
  keepConnectedTask
  , KeepConnectionCommand(..)
  , fire
  , EventSource
  , DisconnectEvent(..)
  , ConsoleCommand(..)
  , parseProducer
) where

import Control.Applicative ((<$), (<|>))
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
import qualified Pipes.ByteString as PBS
import Pipes.Network.TCP as PNT
import System.IO as SIO (hClose, openFile, Handle, withFile, IOMode(WriteMode))
import Data.Text.Encoding
import Data.ByteString.Char8 as DBC8 hiding (isInfixOf, isPrefixOf, snoc, putStrLn) 
import Control.Concurrent.Async
import Parser
import Pipes.Attoparsec
import Pipes.Parse

newtype GoCommand = GoCommand [Text]
data StopCommand = StopCommand
newtype EnterLocationEvent = EnterLocationEvent Int
data PulseEvent = PulseEvent
newtype GoDirectionAction = GoDirectionAction Text

newtype KeepConnectionCommand = KeepConnectionCommand Bool
data DisconnectEvent = DisconnectEvent
type EventSource a = (AddHandler a, a -> IO ())

data ConsoleCommand = UserInput Text

keepConnectedTask :: EventSource ConsoleCommand -> (ByteString -> IO ()) -> MomentIO ()
keepConnectedTask consoleCommandEventSource sendToConsoleAction = do
    (disconnectionEvent, fireDisconnection) <- newEvent
    (bSocket, fireUpdateSocket) <- newBehavior Nothing
    consoleCommandEvent <- fromAddHandler $ fst consoleCommandEventSource
    let keepConnectionEvent = keepConnectionCommandToEvent <$> filterE isKeepConnectionCommand consoleCommandEvent
    bKeepConnection <- stepper False keepConnectionEvent

    (serverEvent, fireServerEvent) <- newEvent

    reactimate $ connectToServer fireDisconnection fireUpdateSocket sendToConsoleAction fireServerEvent <$ whenE (isNothing <$> bSocket) (filterE id keepConnectionEvent)
    reactimate $ connectToServer fireDisconnection fireUpdateSocket sendToConsoleAction fireServerEvent <$ whenE bKeepConnection disconnectionEvent
    reactimate $ sendCommand sendToConsoleAction <$> bSocket <@> ((\(UserInput txt) -> txt) <$> filterE notCommandInput consoleCommandEvent)

    keepLoggedTask sendToConsoleAction bSocket serverEvent

keepLoggedTask :: (ByteString -> IO ()) -> Behavior (Maybe Socket) -> Event ServerEvent -> MomentIO ()
keepLoggedTask sendToConsoleAction bSocket serverEvent = do
    reactimate $ sendCommand sendToConsoleAction <$> bSocket <@> ("5" <$ filterE (== CodepagePrompt) serverEvent)
    reactimate $ sendCommand sendToConsoleAction <$> bSocket <@> ("ладень" <$ filterE (== LoginPrompt) serverEvent)
    reactimate $ sendCommand sendToConsoleAction <$> bSocket <@> ("каркасный" <$ filterE (== PasswordPrompt) serverEvent)
    reactimate $ sendCommand sendToConsoleAction <$> bSocket <@> ("" <$ filterE (== WelcomePrompt) serverEvent)

sendCommand :: (ByteString -> IO ()) -> Maybe Socket -> Text -> IO ()
sendCommand _ (Just sock) txt = NST.send sock $ encodeUtf8 $ snoc txt '\n'
sendCommand action Nothing _ = action "no connection to server"

keepConnectionCommandToEvent :: ConsoleCommand -> Bool
keepConnectionCommandToEvent (UserInput ":conn") = True
keepConnectionCommandToEvent (UserInput ":unconn") = False

isKeepConnectionCommand :: ConsoleCommand -> Bool
isKeepConnectionCommand (UserInput ":conn") = True
isKeepConnectionCommand (UserInput ":unconn") = True
isKeepConnectionCommand _ = False

notCommandInput :: ConsoleCommand -> Bool
notCommandInput (UserInput input) = not $ isPrefixOf ":" input

connectToServer :: Handler DisconnectEvent -> Handler (Maybe Socket) -> (ByteString -> IO ()) -> Handler ServerEvent -> IO ()
connectToServer fireDisconnection updateSocketBehavior sendToConsoleAction fireServerEvent = do
    (sock, addr) <- NST.connectSock "bylins.su" "4000"
    updateSocketBehavior $ Just sock
    (persToConsOut, persToConsIn, sealPersToCons) <- spawn' unbounded
    logFile <- openFile "log" WriteMode
    (persToLogOut, persToLogIn, sealPersToLog) <- spawn' unbounded
    (parseServerTextOut, parseServerTextIn, sealParseServerText) <- spawn' unbounded
    let closeSockOnEof = NST.closeSock sock
    let closeLogFile = liftIO $ hClose logFile
    let fireDisconnectionEvent = liftIO $ fireDisconnection DisconnectEvent
    let seal f = liftIO $ atomically f
    let cleanup = closeLogFile >> liftIO (updateSocketBehavior Nothing) >> fireDisconnectionEvent 
    async $ do runEffect $ (fromSocket sock (2^15) >> closeSockOnEof) >-> toOutput (persToConsOut <> persToLogOut <> parseServerTextOut) >> cleanup
               performGC
    async $ do runEffect $ fromInput persToConsIn >-> sendToConsoleConsumer sendToConsoleAction >> seal sealPersToCons
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

personBotTest :: IO ()
personBotTest = do 
    (addGoCommandHandler, fireGoCommand) <- newAddHandler
    (addStopCommandHandler, fireStopCommand) <- newAddHandler
    (addEnterLocationHandler, fireEnterLocationEvent) <- newAddHandler
    (addPulseEvent, firePulseEvent) <- newAddHandler

    network <- compile $ do
        goCommandEvent <- fromAddHandler addGoCommandHandler
        stopCommandEvent <- fromAddHandler addStopCommandHandler
        enterLocationEvent <- fromAddHandler addEnterLocationHandler
        pulseEvent <- fromAddHandler addPulseEvent

        ifGo <- accumB False $ unions [const True <$ goCommandEvent, const False <$ stopCommandEvent]
        path <- accumB Nothing $ unions [(\(GoCommand path) acc -> Just path) <$> goCommandEvent, removePathHead <$ enterLocationEvent]
        let filteredPulseEvent = whenE ifGo pulseEvent
        let pathEvent = path <@ whenE (bPathNotEmpty path) filteredPulseEvent 
        let goDirectionAction = GoDirectionAction . Prelude.head . fromJust <$> pathEvent

        reactimate $ DTIO.putStrLn "pulse event" <$ pulseEvent
        reactimate $ printDirection <$> goDirectionAction

    actuate network
    firePulseEvent PulseEvent
    fireGoCommand $ GoCommand ["north", "south", "west", "east"]
    firePulseEvent PulseEvent
    fireEnterLocationEvent $ EnterLocationEvent 1
    firePulseEvent PulseEvent
    fireEnterLocationEvent $ EnterLocationEvent 1
    firePulseEvent PulseEvent
    fireEnterLocationEvent $ EnterLocationEvent 1
    firePulseEvent PulseEvent
    fireEnterLocationEvent $ EnterLocationEvent 1
    firePulseEvent PulseEvent
    firePulseEvent PulseEvent
    fireStopCommand StopCommand
    firePulseEvent PulseEvent
    firePulseEvent PulseEvent

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

fire = snd
