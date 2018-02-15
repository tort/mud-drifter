{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Event ( Event(..)
             , PersonCommand(..)
             , ServerEvent(..)
             , EventBus
             , Location(..)
             , LocId
             , LocTitle
             , BodyPart(..)
             , EquipmentItem(..)
             , EquipmentItemState(..)
             ) where

import Pipes.Concurrent
import Data.Text
import Data.ByteString

import Data.Binary
import GHC.Generics (Generic)

instance Binary Event
instance Binary PersonCommand
instance Binary ServerEvent
instance Binary Location
instance Binary BodyPart
instance Binary EquipmentItem
instance Binary EquipmentItemState

data Event = ConsoleInput Text
           | PersonCommand PersonCommand
           | ServerCommand Text
           | ServerInput ByteString
           | ServerEvent ServerEvent
           | ConsoleOutput ByteString
           | ServerDisconnection
           deriving (Eq, Show, Generic)

type EventBus = (Output Event, Input Event)

data PersonCommand = UserInputRedirect Text
               | KeepConn Bool
               | FindLoc Text
               | FindPathFromTo Int Int
               | FindPathToLocId Int
               | FindPathTo Text
               | GoTo Text
               | GoToLocId Int
               deriving (Eq, Show, Generic)

data ServerEvent = CodepagePrompt
                 | LoginPrompt
                 | PasswordPrompt
                 | WelcomePrompt
                 | PostWelcome
                 | LocationEvent Location
                 | MoveEvent Text
                 | DarknessEvent
                 | UnknownServerEvent ByteString
                 | ListEquipment [EquipmentItem]
                 deriving (Eq, Show, Generic)

data BodyPart = Body | Head | Arms | Legs | RightHand | LeftHand | Feet | Waist | RightWrist | LeftWrist | Neck | Shoulders deriving (Eq, Show, Generic)
data EquipmentItem = EquipmentItem (BodyPart, Text, EquipmentItemState) deriving (Eq, Show, Generic)
data EquipmentItemState = Excellent | Good | Bad deriving (Eq, Show, Generic)

data Location = Location { locId :: LocId
                         , locTitle :: LocTitle
                         } deriving (Show, Ord, Generic)
type LocId = Int
type LocTitle = Text

instance Eq Location where
  left == right = locId left == locId right
