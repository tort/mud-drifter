{-# LANGUAGE OverloadedStrings #-}

module Person (
  keepConnectedTask
  , KeepConnectionCommand(..)
  , fire
  , EventSource
  , DisconnectEvent(..)
  , ConsoleCommand(..)
) where

import Control.Applicative ((<$), (<|>))
import Data.Monoid
import Data.Text
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
import Data.ByteString.Char8 hiding (isPrefixOf, snoc, putStrLn)
import Control.Concurrent.Async

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

    reactimate $ connectToServer fireDisconnection fireUpdateSocket sendToConsoleAction <$ whenE (isNothing <$> bSocket) (filterE id keepConnectionEvent)
    reactimate $ connectToServer fireDisconnection fireUpdateSocket sendToConsoleAction <$ whenE bKeepConnection disconnectionEvent
    reactimate $ consoleCommandToIO <$> bSocket <@> filterE notCommandInput consoleCommandEvent

ifTurnOnConnectionBehaviour :: Bool -> Maybe Socket -> Bool
ifTurnOnConnectionBehaviour True Nothing = True
ifTurnOnConnectionBehaviour _ _ = False

consoleCommandToIO :: Maybe Socket -> ConsoleCommand -> IO ()
consoleCommandToIO (Just sock) (UserInput txt) = NST.send sock $ encodeUtf8 $ snoc txt '\n'
consoleCommandToIO Nothing input = putStrLn "no connection to server"

keepConnectionCommandToEvent :: ConsoleCommand -> Bool
keepConnectionCommandToEvent (UserInput ":conn") = True
keepConnectionCommandToEvent (UserInput ":unconn") = False

isKeepConnectionCommand :: ConsoleCommand -> Bool
isKeepConnectionCommand (UserInput ":conn") = True
isKeepConnectionCommand (UserInput ":unconn") = True
isKeepConnectionCommand _ = False

notCommandInput :: ConsoleCommand -> Bool
notCommandInput (UserInput input) = not $ isPrefixOf ":" input

connectToServer :: Handler DisconnectEvent -> Handler (Maybe Socket) -> (ByteString -> IO ()) -> IO ()
connectToServer fireDisconnection updateSocketBehavior sendToConsoleAction = do
    (sock, addr) <- NST.connectSock "bylins.su" "4000"
    updateSocketBehavior $ Just sock
    (persToConsOut, persToConsIn, sealPersToCons) <- spawn' unbounded
    logFile <- openFile "log" WriteMode
    (persToLogOut, persToLogIn, sealPersToLog) <- spawn' unbounded
    let closeSockOnEof = NST.closeSock sock
    let closeLogFile = liftIO $ hClose logFile
    let fireDisconnectionEvent = liftIO $ fireDisconnection DisconnectEvent
    let seal f = liftIO $ atomically f
    let cleanup = closeLogFile >> liftIO (updateSocketBehavior Nothing) >> fireDisconnectionEvent 
    async $ do runEffect $ (fromSocket sock (2^15) >> closeSockOnEof) >-> toOutput (persToConsOut <> persToLogOut) >> cleanup
               performGC
    async $ do runEffect $ fromInput persToConsIn >-> PBS.stdout >> seal sealPersToCons
               performGC
    async $ do runEffect $ fromInput persToLogIn >-> PBS.toHandle logFile >> seal sealPersToLog
               performGC
    return ()

sendToConsoleConsumer :: (ByteString -> IO ()) -> Consumer ByteString IO ()
sendToConsoleConsumer action = do
    text <- await
    liftIO $ action text
    sendToConsoleConsumer action

negatedImplication :: (Bool, Bool) -> Bool
negatedImplication (False, True) = True
negatedImplication (False, False) = False
negatedImplication (True, True) = False
negatedImplication (True, False) = False

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
