{-# LANGUAGE OverloadedStrings #-}

module Person (
  personHandle
  , personBot
) where

import Data.Text
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Maybe

newtype GoCommand = GoCommand [String]
data StopCommand = StopCommand
newtype EnterLocationEvent = EnterLocationEvent Int
data PulseEvent = PulseEvent
newtype GoDirectionAction = GoDirectionAction String

personHandle :: Text -> Text
personHandle ":connect" = ":connected"
personHandle text = text

personBot :: IO ()
personBot = do 
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

removePathHead :: Maybe [String] -> Maybe [String]
removePathHead Nothing = Nothing
removePathHead (Just []) = Nothing
removePathHead x@(Just xs) = fmap Prelude.tail x

bPathNotEmpty :: Behavior (Maybe [String]) -> Behavior Bool
bPathNotEmpty = fmap pathNotEmpty

pathNotEmpty :: Maybe [String] -> Bool
pathNotEmpty Nothing = False
pathNotEmpty (Just []) = False
pathNotEmpty (Just xs) = True
