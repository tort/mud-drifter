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
             , UserCommand(..)
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
             , MobRoomDesc(..)
             , ItemRoomDesc(..)
             , ItemAccusative(..)
             , ItemNominative(..)
             , MobGenitive(..)
             , ItemGenitive(..)
             , MobNominative(..)
             , ShowVal(..)
             , isServerEvent
             , isMoveEvent
             , isConsoleInput
             , isUserCommand
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

newtype LocationId = LocationId Int deriving (Eq, Ord, Show, Generic)
newtype LocationTitle = LocationTitle Text deriving (Eq, Ord, Show, Generic)

data Location = Location { _locationId :: LocationId
                         , _locationTitle :: LocationTitle
                         } deriving (Show, Ord, Generic)

instance Eq Location where
  left == right = _locationId left == _locationId right

instance P.Show RoomDir where
  show North = "север"
  show South = "юг"
  show West = "запад"
  show East = "восток"
  show Up = "вверх"
  show Down = "вниз"

instance Binary Event
instance Binary UserCommand
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
instance Binary ItemAccusative
instance Binary ItemNominative
instance Binary MobGenitive
instance Binary ItemGenitive
instance Binary MobNominative

data Event = ConsoleInput Text
           | ConsoleOutput ByteString
           | UserCommand UserCommand
           | SendToServer Text
           | ServerInput ByteString
           | ServerEvent { _serverEvent :: ServerEvent }
           | ServerClosedChannel
           | ServerIOException
           | UserInputIOException
           | PulseEvent
           | TravelRequest [LocationId]
           | TravelFailure
           deriving (Eq, Show, Generic)

type EventBus = (Output Event, Input Event)

data UserCommand = ServerCommand Text
               | Connect
               | Zap
               | FindLoc Text
               | FindPathFromTo LocationId LocationId
               | FindPathToLocId LocationId
               | FindPathTo Text
               | GoTo Text
               | GoToLocId LocationId
               | Equip
               | WhereMob Text
               | WhereObject Text
               deriving (Eq, Show, Generic)

data ServerEvent = CodepagePrompt
                 | LoginPrompt
                 | PasswordPrompt
                 | WelcomePrompt
                 | PostWelcome
                 | LocationEvent { _location :: Location, _objects :: [ItemRoomDesc], _mobs :: [MobRoomDesc] }
                 | MoveEvent Text
                 | DarknessEvent
                 | UnknownServerEvent ByteString
                 | ListEquipmentEvent [(EquippedItem, ItemState)]
                 | ListInventoryEvent [(ItemNominative, ItemState)]
                 | ItemStatsEvent ItemStats
                 | ShopListItemEvent ItemNominative Price
                 | PromptEvent
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
                 deriving (Eq, Show, Generic, Ord)

data Slot = Body | Head | Arms | Legs | Wield | Hold | DualWield | Hands | Feet | Waist | RightWrist | LeftWrist | Neck | Shoulders deriving (Eq, Show, Generic, Ord)
data EquippedItem = EquippedItem Slot ItemNominative deriving (Eq, Show, Generic, Ord)
data ItemState = Excellent | VeryGood | Good | Bad deriving (Eq, Show, Generic, Ord)
data ItemStats = Weapon ItemNominative WeaponClass [Slot] AvgDamage | Armor ItemNominative [Slot] AC ArmorVal deriving (Eq, Show, Generic, Ord)
type AvgDamage = Double
data WeaponClass = LongBlade | ShortBlade | Axe | Dagger | Spear | Club | Dual | Other deriving (Eq, Show, Generic, Ord)
type AC = Int
type ArmorVal = Int
type Price = Int
data MobStats = EmptyMobStats deriving (Eq, Show, Generic)
newtype MobRoomDesc = MobRoomDesc { _text :: Text } deriving (Eq, Ord, Show, Generic)
data RoomDir = North | South | East | West | Up | Down deriving (Eq, Generic, Ord)

newtype MobNominative = MobNominative Text deriving (Eq, Show, Ord, Generic)
newtype MobAlias = MobAlias Text deriving (Eq, Show)
newtype MobGenitive = MobGenitive Text deriving (Eq, Show, Ord, Generic)
data Mob = Mob { _roomDesc :: MobRoomDesc
               , _name :: MobNominative
               , _handleAlias :: MobAlias
               , _stats :: Maybe MobStats
               } deriving (Show)

newtype ItemRoomDesc = ItemRoomDesc { _text :: Text } deriving (Eq, Ord, Show, Generic)
newtype ItemAccusative = ItemAccusative Text deriving (Eq, Show, Generic, Ord)
newtype ItemNominative = ItemNominative Text deriving (Eq, Show, Generic, Ord)
newtype ItemGenitive = ItemGenitive Text deriving (Eq, Show, Generic, Ord)
newtype ItemAlias = ItemAlias Text deriving (Eq, Show)
data Item = Item { _roomDesc :: ItemRoomDesc
                 , _nominative :: ItemNominative
                 , _accusative :: ItemAccusative
                 , _alias :: ItemAlias
                 , _stats :: ItemStats
                 } deriving (Show)

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

derive makeIs ''UserCommand
derive makeIs ''Location
derive makeIs ''Slot
derive makeIs ''EquippedItem
derive makeIs ''ItemState
derive makeIs ''ItemStats
derive makeIs ''WeaponClass
derive makeIs ''RoomDir
derive makeIs ''ServerEvent
derive makeIs ''Event

makeFieldsNoPrefix ''UserCommand
makeFieldsNoPrefix ''Location
makeFieldsNoPrefix ''Slot
makeFieldsNoPrefix ''EquippedItem
makeFieldsNoPrefix ''ItemState
makeFieldsNoPrefix ''ItemStats
makeFieldsNoPrefix ''WeaponClass
makeFieldsNoPrefix ''RoomDir
makeFieldsNoPrefix ''ServerEvent
makeFieldsNoPrefix ''Event
makeFieldsNoPrefix ''Mob
