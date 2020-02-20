{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

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
             , InventoryItem(..)
             , Mob(..)
             , Item(..)
             , WeaponClass(..)
             , RoomDir(..)
             , RoomExit(..)
             , ObjRef(..)
             , ObjCase(..)
             , ObjCases(..)
             , MobRoomDesc(..)
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
instance Binary RoomExit
instance Binary (ObjRef Item Nominative)
instance Binary (ObjRef Item Accusative)
instance Binary (ObjRef Item Genitive)
instance Binary (ObjRef Item InRoomDesc)
instance Binary (ObjRef Item Alias)
instance Binary (ObjRef Mob Nominative)
instance Binary (ObjRef Mob Accusative)
instance Binary (ObjRef Mob Genitive)
instance Binary (ObjRef Mob InRoomDesc)
instance Binary (ObjRef Mob Alias)
instance Binary InventoryItem

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
                 | LocationEvent { _location :: Location, _objects :: [ObjRef Item InRoomDesc], _mobs :: [ObjRef Mob InRoomDesc], _exits :: [RoomExit] }
                 | MoveEvent Text
                 | DarknessEvent
                 | UnknownServerEvent ByteString
                 | ListEquipmentEvent [(EquippedItem, ItemState)]
                 | ListInventoryEvent [(ObjRef Item Nominative, ItemState)]
                 | ItemStatsEvent ItemStats
                 | ShopListItemEvent (ObjRef Item Nominative) Price
                 | PromptEvent Int Int
                 | FightPromptEvent (ObjRef Mob Nominative) (ObjRef Mob Nominative)
                 | ObstacleEvent RoomDir Text
                 | CantGoDir
                 | DarkInDirection RoomDir
                 | GlanceEvent RoomDir LocationTitle [ObjRef Mob Nominative]
                 | PickItemEvent (ObjRef Item Accusative)
                 | ItemInTheRoom (ObjRef Item InRoomDesc)
                 | LootItem (ObjRef Item Accusative) (ObjRef Mob Genitive)
                 | LootMoney (ObjRef Mob Genitive)
                 | TakeFromContainer (ObjRef Item Accusative) (ObjRef Item Genitive)
                 | TakeInRightHand (ObjRef Item Accusative)
                 | TakeInLeftHand (ObjRef Item Accusative)
                 | TakeInBothHands (ObjRef Item Accusative)
                 | MobGaveYouItem (ObjRef Mob Nominative) (ObjRef Item Accusative)
                 | Drink Text Text
                 | Eat Text
                 | DrinkFromAbsentObject
                 | ItemAbsent Text
                 | NotHungry
                 | NotThirsty
                 | LiquidContainerIsEmpty
                 | ExamineContainer { _name :: Text, _items :: [InventoryItem] }
                 | MobRipEvent
                 | ImBashedEvent
                 | MyStats Int Int
                 | ParseError ByteString
                 deriving (Eq, Generic, Ord, Show)

data Slot = Body | Head | Arms | Legs | Wield | Hold | DualWield | Hands | Feet | Waist | RightWrist | LeftWrist | Neck | Shoulders
  deriving (Eq, Generic, Ord, Show)
data EquippedItem = EquippedItem Slot (ObjRef Item Nominative)
  deriving (Eq, Generic, Ord, Show)
data ItemState = Excellent | VeryGood | Good | Bad
  deriving (Eq, Generic, Ord, Show)
data ItemStats = Weapon (ObjRef Item Nominative) WeaponClass [Slot] AvgDamage | Armor (ObjRef Item Nominative) [Slot] AC ArmorVal
  deriving (Eq, Generic, Ord, Show)
type AvgDamage = Double
data WeaponClass = LongBlade | ShortBlade | Axe | Dagger | Spear | Club | Dual | Other
  deriving (Eq, Generic, Ord, Show)
type AC = Int
type ArmorVal = Int
type Price = Int

data InventoryItem = Single (ObjRef Item Nominative) ItemState | Multiple (ObjRef Item Nominative) Int
  deriving (Eq, Ord, Generic, Show)

data ObjCase = Nominative
             | Accusative
             | Genitive
             | InRoomDesc
             | Alias
             deriving (Eq, Ord, Show)

newtype ObjRef a (b :: ObjCase) = ObjRef { unObjRef :: Text }
  deriving (Eq, Ord, Generic, Show)

type ObjCases a = Map ObjCase Text

type MobRoomDesc = ObjRef Mob InRoomDesc

instance TextShow (ObjRef a b) where
  showt = unObjRef

data RoomDir = North | South | East | West | Up | Down deriving (Eq, Generic, Ord, Show)
data RoomExit = OpenExit RoomDir | ClosedExit RoomDir deriving (Eq, Generic, Ord, Show)

data Mob
data Item

class ShowVal a where
  showVal :: a -> Text

instance ShowVal LocationId where
  showVal (LocationId id) = show id

instance ShowVal LocationTitle where
  showVal (LocationTitle text) = text

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
