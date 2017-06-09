{-# LANGUAGE OverloadedStrings #-}

module Person (
  keepConnectedTask
  , KeepConnectionCommand(..)
  , fire
  , EventSource
  , DisconnectEvent(..)
) where

import Data.Text
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Maybe
import Pipes.Concurrent
import Data.ByteString.Char8
import Prelude hiding (putStrLn)
import Pipes
import Control.Monad
import Data.Monoid
import Pipes.Network.TCP
import System.IO as SIO (hClose, openFile, Handle, withFile, IOMode(WriteMode))
import Pipes.ByteString (ByteString, stdout, toHandle)

newtype GoCommand = GoCommand [ByteString]
data StopCommand = StopCommand
newtype EnterLocationEvent = EnterLocationEvent Int
data PulseEvent = PulseEvent
newtype GoDirectionAction = GoDirectionAction ByteString

newtype KeepConnectionCommand = KeepConnectionCommand Bool
data DisconnectEvent = DisconnectEvent
type EventSource a = (AddHandler a, a -> IO ())
newtype UserInput = UserInput ByteString 

keepConnectedTask :: EventSource KeepConnectionCommand -> EventSource DisconnectEvent -> IO () -> MomentIO ()
keepConnectedTask keepConnectionEventSource disconnectEventSource connectToMud = do
    disconnectionEvent <- fromAddHandler $ fst disconnectEventSource
    keepConnectionEvent <- fromAddHandler $ fst keepConnectionEventSource

    let ifKeepConnection = (\(KeepConnectionCommand b) -> (\pair -> (snd pair, b))) <$> keepConnectionEvent
    oldAndNewEvent <- accumE (False, False) ifKeepConnection 
    bKeepConnection <- stepper False $ fmap (\(KeepConnectionCommand b) -> b) keepConnectionEvent

    reactimate $ connectToMud <$ filterE negatedImplication oldAndNewEvent
    reactimate $ connectToMud <$ whenE bKeepConnection disconnectionEvent

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

        ifGo <- accumB False $ unions $ [(\x -> True) <$ goCommandEvent, (\x -> False) <$ stopCommandEvent]
        path <- accumB Nothing $ unions $ [(\(GoCommand path) acc -> Just path) <$> goCommandEvent, removePathHead <$ enterLocationEvent]
        let filteredPulseEvent = whenE ifGo pulseEvent
        let pathEvent = path <@ whenE (bPathNotEmpty path) filteredPulseEvent 
        let goDirectionAction = (\p -> GoDirectionAction $ Prelude.head $ fromJust p) <$> pathEvent

        reactimate $ putStrLn "pulse event" <$ pulseEvent
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
printDirection (GoDirectionAction direction) = putStrLn direction

removePathHead :: Maybe [ByteString] -> Maybe [ByteString]
removePathHead Nothing = Nothing
removePathHead (Just []) = Nothing
removePathHead x@(Just xs) = fmap Prelude.tail x

bPathNotEmpty :: Behavior (Maybe [ByteString]) -> Behavior Bool
bPathNotEmpty = fmap pathNotEmpty

pathNotEmpty :: Maybe [ByteString] -> Bool
pathNotEmpty Nothing = False
pathNotEmpty (Just []) = False
pathNotEmpty (Just xs) = True

fire = snd
