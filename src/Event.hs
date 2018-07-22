{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Event ( Event(..)
             , PersonCommand(..)
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
instance Binary Slot
instance Binary EquippedItem
instance Binary ItemState
instance Binary Item
instance Binary WeaponClass

data Event = ConsoleInput Text
           | PersonCommand PersonCommand
           | ServerCommand Text
           | ServerInput ByteString
           | ServerEvent ServerEvent
           | ConsoleOutput ByteString
           | ServerDisconnection
           | PulseEvent
           | TravelRequest [LocId]
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

data Location = Location { locId :: LocId
                         , locTitle :: LocTitle
                         } deriving (Show, Ord, Generic)
type LocId = Int
type LocTitle = Text

instance Eq Location where
  left == right = locId left == locId right
