{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Event ( Event(..)
             , UserCommand(..)
             , ServerEvent(..)
             , EventBus
             , Location(..)
             , LocId
             , LocTitle
             , Slot(..)
             , EquippedItem(..)
             , ItemState(..)
             , Item(..)
             , WeaponClass(..)
             , RoomDir(..)
             ) where

import Pipes.Concurrent
import Data.Text
import Data.ByteString

import Data.Binary
import GHC.Generics (Generic)

instance Binary Event
instance Binary UserCommand
instance Binary ServerEvent
instance Binary Location
instance Binary Slot
instance Binary EquippedItem
instance Binary ItemState
instance Binary Item
instance Binary WeaponClass
instance Binary RoomDir

data Event = ConsoleInput Text
           | ConsoleOutput ByteString
           | UserCommand UserCommand
           | SendToServer Text
           | ServerInput ByteString
           | ServerEvent ServerEvent
           | ServerClosedChannel
           | ServerIOException
           | PulseEvent
           | TravelRequest [LocId]
           | TravelFailure
           deriving (Eq, Show, Generic)

type EventBus = (Output Event, Input Event)

data UserCommand = ServerCommand Text
               | Connect
               | Zap
               | FindLoc Text
               | FindPathFromTo Int Int
               | FindPathToLocId Int
               | FindPathTo Text
               | GoTo Text
               | GoToLocId Int
               | Equip
               deriving (Eq, Show, Generic)

data ServerEvent = CodepagePrompt
                 | LoginPrompt
                 | PasswordPrompt
                 | WelcomePrompt
                 | PostWelcome
                 | LocationEvent Location [RoomObject]
                 | MoveEvent Text
                 | DarknessEvent
                 | UnknownServerEvent ByteString
                 | ListEquipmentEvent [(EquippedItem, ItemState)]
                 | ListInventoryEvent [(Text, ItemState)]
                 | ItemStatsEvent Item
                 | ShopListItemEvent ItemName Price
                 | PromptEvent
                 | ObstacleEvent RoomDir Text
                 | CantGoDir
                 | DarkInDirection RoomDir
                 | GlanceEvent RoomDir LocTitle [MobShort]
                 deriving (Eq, Show, Generic)

data Slot = Body | Head | Arms | Legs | Wield | Hold | DualWield | Hands | Feet | Waist | RightWrist | LeftWrist | Neck | Shoulders deriving (Eq, Show, Generic, Ord)
data EquippedItem = EquippedItem Slot Text deriving (Eq, Show, Generic)
data ItemState = Excellent | VeryGood | Good | Bad deriving (Eq, Show, Generic)
data Item = Weapon Text WeaponClass [Slot] AvgDamage | Armor Text [Slot] AC ArmorVal deriving (Eq, Show, Generic, Ord)
type AvgDamage = Double
data WeaponClass = LongBlade | ShortBlade | Axe | Dagger | Spear | Club | Dual | Other deriving (Eq, Show, Generic, Ord)
type AC = Int
type ArmorVal = Int
type ItemName = Text
type Price = Int
type RoomObject = Text
type MobShort = Text
data RoomDir = North | South | East | West | Up | Down deriving (Eq, Generic)

instance Show RoomDir where
  show North = "север"
  show South = "юг"
  show West = "запад"
  show East = "восток"
  show Up = "вверх"
  show Down = "вниз"

data Location = Location { locId :: LocId
                         , locTitle :: LocTitle
                         } deriving (Show, Ord, Generic)
type LocId = Int
type LocTitle = Text

instance Eq Location where
  left == right = locId left == locId right
