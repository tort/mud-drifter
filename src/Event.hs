{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Event ( Event(..)
             , ServerEvent(..)
             , EventBus
             , Location(..)
             , LocationId(..)
             , LocationTitle(..)
             , Slot(..)
             , EquippedItem(..)
             , ItemState(..)
             , ItemStats(..)
             , Mob(..)
             , WeaponClass(..)
             , RoomDir(..)
             , RoomExit(..)
             , MobRoomDesc(..)
             , ItemRoomDesc(..)
             , ItemAccusative(..)
             , ItemNominative(..)
             , MobGenitive(..)
             , ItemGenitive(..)
             , MobNominative(..)
             , ShowVal(..)
             , Result(..)
             , isServerEvent
             , isMoveEvent
             , isConsoleInput
             , isShopListItemEvent
             , isLocationEvent
             , isItemStatsEvent
             , isPromptEvent
             , location
             , locationId
             , locationTitle
             , objects
             , mobs
             , serverEvent
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

import TextShow
import TextShow.Generic

data Result a = Success a | Failure Text
  deriving (Eq, Generic)

newtype LocationId = LocationId Int
  deriving (Eq, Ord, Generic, Show)

instance TextShow LocationId where
  showt (LocationId n) = showt n

newtype LocationTitle = LocationTitle Text
  deriving (Eq, Ord, Generic, Show)

data Location = Location { _locationId :: LocationId
                         , _locationTitle :: LocationTitle
                         } deriving (Ord, Generic, Show)

instance TextShow Location where
  showt (Location (LocationId locId) (LocationTitle title)) = showt locId <> ": " <> title

instance Eq Location where
  left == right = _locationId left == _locationId right

instance TextShow RoomDir where
  showt North = "север"
  showt South = "юг"
  showt West = "запад"
  showt East = "восток"
  showt Up = "вверх"
  showt Down = "вниз"

instance Binary Event
instance Binary ServerEvent
instance Binary Location
instance Binary LocationId
instance Binary LocationTitle
instance Binary Slot
instance Binary EquippedItem
instance Binary ItemState
instance Binary ItemStats
instance Binary WeaponClass
instance Binary RoomDir
instance Binary ItemRoomDesc
instance Binary MobRoomDesc
instance Binary RoomExit
instance Binary ItemAccusative
instance Binary ItemNominative
instance Binary MobGenitive
instance Binary ItemGenitive
instance Binary MobNominative

data Event = ConsoleInput Text
           | ConsoleOutput ByteString
           | SendToServer Text
           | ServerInput ByteString
           | ServerEvent { _serverEvent :: ServerEvent }
           | ServerClosedChannel
           | ServerIOException
           | UserInputIOException
           | PulseEvent
           | TravelRequest [LocationId]
           | TravelFailure
           deriving (Eq, Generic, Show)

type EventBus = (Output Event, Input Event)

data ServerEvent = CodepagePrompt
                 | LoginPrompt
                 | PasswordPrompt
                 | WelcomePrompt
                 | PostWelcome
                 | LocationEvent { _location :: Location, _objects :: [ItemRoomDesc], _mobs :: [MobRoomDesc], _exits :: [RoomExit] }
                 | MoveEvent Text
                 | DarknessEvent
                 | UnknownServerEvent ByteString
                 | ListEquipmentEvent [(EquippedItem, ItemState)]
                 | ListInventoryEvent [(ItemNominative, ItemState)]
                 | ItemStatsEvent ItemStats
                 | ShopListItemEvent ItemNominative Price
                 | PromptEvent
                 | FightPromptEvent
                 | ObstacleEvent RoomDir Text
                 | CantGoDir
                 | DarkInDirection RoomDir
                 | GlanceEvent RoomDir LocationTitle [MobRoomDesc]
                 | PickItemEvent ItemAccusative
                 | ItemInTheRoom ItemRoomDesc
                 | LootCorpse ItemAccusative MobGenitive
                 | TakeFromContainer ItemAccusative ItemGenitive
                 | TakeInRightHand ItemAccusative
                 | TakeInLeftHand ItemAccusative
                 | TakeInBothHands ItemAccusative
                 | MobGaveYouItem MobNominative ItemAccusative
                 | ParseError ByteString
                 deriving (Eq, Generic, Ord, Show)

data Slot = Body | Head | Arms | Legs | Wield | Hold | DualWield | Hands | Feet | Waist | RightWrist | LeftWrist | Neck | Shoulders
  deriving (Eq, Generic, Ord, Show)
data EquippedItem = EquippedItem Slot ItemNominative
  deriving (Eq, Generic, Ord, Show)
data ItemState = Excellent | VeryGood | Good | Bad
  deriving (Eq, Generic, Ord, Show)
data ItemStats = Weapon ItemNominative WeaponClass [Slot] AvgDamage | Armor ItemNominative [Slot] AC ArmorVal
  deriving (Eq, Generic, Ord, Show)
type AvgDamage = Double
data WeaponClass = LongBlade | ShortBlade | Axe | Dagger | Spear | Club | Dual | Other
  deriving (Eq, Generic, Ord, Show)
type AC = Int
type ArmorVal = Int
type Price = Int
data MobStats = EmptyMobStats deriving (Eq, Generic)
newtype MobRoomDesc = MobRoomDesc { _text :: Text }
  deriving (Eq, Ord, Generic, Show)
data RoomDir = North | South | East | West | Up | Down deriving (Eq, Generic, Ord, Show)
data RoomExit = OpenExit RoomDir | ClosedExit RoomDir deriving (Eq, Generic, Ord, Show)

newtype MobNominative = MobNominative Text
  deriving (Eq, Ord, Generic, Show)
newtype MobAlias = MobAlias Text deriving (Eq)
newtype MobGenitive = MobGenitive Text
  deriving (Eq, Ord, Generic, Show)
data Mob = Mob { _roomDesc :: MobRoomDesc
               , _name :: MobNominative
               , _handleAlias :: MobAlias
               , _stats :: Maybe MobStats
               }

newtype ItemRoomDesc = ItemRoomDesc { _text :: Text }
  deriving (Eq, Ord, Generic, Show)
newtype ItemAccusative = ItemAccusative Text
  deriving (Eq, Generic, Ord, Show)
newtype ItemNominative = ItemNominative Text
  deriving (Eq, Generic, Ord, Show)
newtype ItemGenitive = ItemGenitive Text
  deriving (Eq, Generic, Ord, Show)
newtype ItemAlias = ItemAlias Text deriving (Eq)
data Item = Item { _roomDesc :: ItemRoomDesc
                 , _nominative :: ItemNominative
                 , _accusative :: ItemAccusative
                 , _alias :: ItemAlias
                 , _stats :: ItemStats
                 }

makeFieldsNoPrefix ''Item

instance Eq Mob where
  left == right = left^.roomDesc == right^.roomDesc

class ShowVal a where
  showVal :: a -> Text

instance ShowVal LocationId where
  showVal (LocationId id) = show id

instance ShowVal LocationTitle where
  showVal (LocationTitle text) = text

instance ShowVal ItemRoomDesc where
  showVal (ItemRoomDesc text) = text

instance ShowVal MobRoomDesc where
  showVal (MobRoomDesc text) = text

instance ShowVal MobGenitive where
  showVal (MobGenitive text) = text

instance ShowVal MobNominative where
  showVal (MobNominative text) = text

instance ShowVal ItemAccusative where
  showVal (ItemAccusative text) = text

instance ShowVal ItemGenitive where
  showVal (ItemGenitive text) = text

derive makeIs ''Location
derive makeIs ''LocationId
derive makeIs ''LocationTitle
derive makeIs ''Slot
derive makeIs ''EquippedItem
derive makeIs ''ItemState
derive makeIs ''ItemStats
derive makeIs ''WeaponClass
derive makeIs ''RoomDir
derive makeIs ''ServerEvent
derive makeIs ''Event

makeFieldsNoPrefix ''Location
makeFieldsNoPrefix ''LocationId
makeFieldsNoPrefix ''LocationTitle
makeFieldsNoPrefix ''Slot
makeFieldsNoPrefix ''EquippedItem
makeFieldsNoPrefix ''ItemState
makeFieldsNoPrefix ''ItemStats
makeFieldsNoPrefix ''WeaponClass
makeFieldsNoPrefix ''RoomDir
makeFieldsNoPrefix ''ServerEvent
makeFieldsNoPrefix ''Event
makeFieldsNoPrefix ''Mob
