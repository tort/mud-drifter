{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Event ( Event(..)
             , UserCommand(..)
             , ServerEvent(..)
             , EventBus
             , Location(..)
             , LocationId(..)
             , LocationTitle(..)
             , Slot(..)
             , EquippedItem(..)
             , ItemState(..)
             , Item(..)
             , WeaponClass(..)
             , RoomDir(..)
             , MobShortDesc(..)
             , RoomObject(..)
             , isMoveEvent
             , isConsoleInput
             , isUserCommand
             , isShopListItemEvent
             , isLocationEvent
             , location
             , locationId
             , locationTitle
             , objects
             , mobs
             , serverEvent
             , val
             , text
             ) where

import qualified Prelude as P
import Protolude hiding (Location, Down, Up, Left, Right, Dual)
import Pipes.Concurrent
import Data.Text
import Data.ByteString

import Data.Binary
import GHC.Generics (Generic)

import Data.DeriveTH
import Control.Lens hiding ((&))

instance Binary Event
instance Binary UserCommand
instance Binary ServerEvent
instance Binary Location
instance Binary LocationId
instance Binary LocationTitle
instance Binary Slot
instance Binary EquippedItem
instance Binary ItemState
instance Binary Item
instance Binary WeaponClass
instance Binary RoomDir
instance Binary RoomObject
instance Binary MobShortDesc

data Event = ConsoleInput Text
           | ConsoleOutput ByteString
           | UserCommand UserCommand
           | SendToServer Text
           | ServerInput ByteString
           | ServerEvent { _serverEvent :: ServerEvent }
           | ServerClosedChannel
           | ServerIOException
           | PulseEvent
           | TravelRequest [LocationId]
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
               | GoToLocId LocationId
               | Equip
               deriving (Eq, Show, Generic)

data ServerEvent = CodepagePrompt
                 | LoginPrompt
                 | PasswordPrompt
                 | WelcomePrompt
                 | PostWelcome
                 | LocationEvent { _location :: Location, _objects :: [RoomObject], _mobs :: [MobShortDesc] }
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
                 | GlanceEvent RoomDir LocationTitle [MobShortDesc]
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
newtype RoomObject = RoomObject { _text :: Text } deriving (Eq, Show, Generic)
newtype MobShortDesc = MobShortDesc { _text :: Text } deriving (Eq, Ord, Show, Generic)
data RoomDir = North | South | East | West | Up | Down deriving (Eq, Generic)

data Mob = Mob { _shortDesc :: MobShortDesc
               , _name :: Maybe MobName
               , _tag :: Maybe Tag
               , _locations :: [LocationId]
               } deriving (Show)

instance Eq Mob where
  left == right = _shortDesc left == _shortDesc right

newtype MobName = MobName Text deriving (Eq, Show)
newtype Tag = Tag Text deriving (Eq, Show)

instance P.Show RoomDir where
  show North = "север"
  show South = "юг"
  show West = "запад"
  show East = "восток"
  show Up = "вверх"
  show Down = "вниз"

data Location = Location { _locationId :: LocationId
                         , _locationTitle :: LocationTitle
                         } deriving (Show, Ord, Generic)

newtype LocationId = LocationId { _val :: Int } deriving (Eq, Ord, Show, Generic)
newtype LocationTitle = LocationTitle { _text :: Text } deriving (Eq, Ord, Show, Generic)

instance Eq Location where
  left == right = _locationId left == _locationId right

derive makeIs ''UserCommand
derive makeIs ''Location
derive makeIs ''Slot
derive makeIs ''EquippedItem
derive makeIs ''ItemState
derive makeIs ''Item
derive makeIs ''WeaponClass
derive makeIs ''RoomDir
derive makeIs ''ServerEvent
derive makeIs ''Event

makeFieldsNoPrefix ''UserCommand
makeFieldsNoPrefix ''Location
makeFieldsNoPrefix ''LocationId
makeFieldsNoPrefix ''LocationTitle
makeFieldsNoPrefix ''Slot
makeFieldsNoPrefix ''EquippedItem
makeFieldsNoPrefix ''ItemState
makeFieldsNoPrefix ''Item
makeFieldsNoPrefix ''WeaponClass
makeFieldsNoPrefix ''RoomDir
makeFieldsNoPrefix ''ServerEvent
makeFieldsNoPrefix ''Event
