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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

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
             , ObjType(..)
             , WeaponClass(..)
             , RoomDir(..)
             , RoomExit(..)
             , ObjRef(..)
             , ObjCase(..)
             , MobRoomDesc(..)
             , MobInTheRoom(..)
             , MobStats(..)
             , EverAttacked(..)
             , NameCases(..)
             , ShowVal(..)
             , Result(..)
             , location
             , locationId
             , locationTitle
             , objects
             , mobs
             , serverEvent
             , nameCases
             , inRoomDesc
             , nominative
             , genitive
             , accusative
             , dative
             , instrumental
             , prepositional
             , everAttacked
             , _CodepagePrompt
             , _CheckNominative
             , _CheckGenitive
             , _CheckAccusative
             , _CheckDative
             , _CheckInstrumental
             , _CheckPrepositional 
             , _LocationEvent
             , _MoveEvent
             , _ItemStatsEvent
             , _FightPromptEvent
             , _ShopListItemEvent
             , _PromptEvent
             , _TakeFromContainer
             , _ServerEvent
             , _UnknownServerEvent
             , _MobRipEvent
             ) where

import qualified Prelude as P
import Protolude hiding (Location, Down, Up, Left, Right, Dual)
import Pipes.Concurrent
import Data.Text
import qualified Data.Text as T
import Data.ByteString

import Data.Binary
import GHC.Generics (Generic)

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

instance TextShow ServerEvent where
  showt (MobGaveYouItem from to) = "MobGaveYouItem [" <> showt from <> "] [" <> showt to <> "]"
  showt (TakeFromContainer from to) = "TakeFromContainer [" <> showt from <> "] [" <> showt to <> "]"
  showt x = T.pack . show $ x

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
instance Binary MobInTheRoom
instance Binary (ObjRef Item Nominative)
instance Binary (ObjRef Item Accusative)
instance Binary (ObjRef Item Genitive)
instance Binary (ObjRef Item Dative)
instance Binary (ObjRef Item Instrumental)
instance Binary (ObjRef Item Prepositional)
instance Binary (ObjRef Item InRoomDesc)
instance Binary (ObjRef Item Alias)
instance Binary (ObjRef Mob Nominative)
instance Binary (ObjRef Mob Accusative)
instance Binary (ObjRef Mob Genitive)
instance Binary (ObjRef Mob Dative)
instance Binary (ObjRef Mob Instrumental)
instance Binary (ObjRef Mob Prepositional)
instance Binary (ObjRef Mob InRoomDesc)
instance Binary (ObjRef Mob Alias)
instance Binary InventoryItem

data Event = ConsoleInput Text
           | ConsoleOutput ByteString
           | SendToServer Text
           | SendOnPulse Int Text
           | LockPulse Int
           | ReleasePulse Int
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
                 | FightPromptEvent { _me :: ObjRef Mob Nominative, _target :: ObjRef Mob Nominative }
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
                 | MobWentOut (ObjRef Mob Nominative)
                 | Drink Text Text
                 | Eat Text
                 | DrinkFromAbsentObject
                 | ItemAbsent Text
                 | NotHungry
                 | NotThirsty
                 | LiquidContainerIsEmpty
                 | ExamineContainer { _name :: Text, _items :: [InventoryItem] }
                 | MobRipEvent (ObjRef Mob Nominative)
                 | ExpUpEvent
                 | ImBashedEvent
                 | MyStats Int Int
                 | ParseError ByteString
                 | IHitMobEvent (ObjRef Mob Accusative)
                 | CheckNominative (ObjRef Mob Nominative)
                 | CheckGenitive (ObjRef Mob Genitive)
                 | CheckAccusative (ObjRef Mob Accusative)
                 | CheckDative (ObjRef Mob Dative)
                 | CheckInstrumental (ObjRef Mob Instrumental)
                 | CheckPrepositional (ObjRef Mob Prepositional)
                 | MobEnteredLocation
                 | HitEvent (ObjRef Mob Nominative) (ObjRef Mob Accusative)
                 | EndOfLogEvent
                 deriving (Eq, Generic, Ord, Show)

data MobInTheRoom = MobDescRef { _unMobDescRef :: ObjRef Mob InRoomDesc } | MobNomRef { _unMobNomRef :: ObjRef Mob Nominative }
  deriving (Eq, Ord, Show, Generic)

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
             | Genitive
             | Dative
             | Accusative
             | Instrumental
             | Prepositional
             | InRoomDesc
             | Alias
             deriving (Eq, Ord, Show)

data ObjType = Mob | Item deriving (Eq, Ord, Show)

newtype ObjRef (a :: ObjType) (b :: ObjCase) = ObjRef { unObjRef :: Text }
  deriving (Eq, Ord, Generic, Show)

instance Semigroup (ObjRef a b) where
  left <> right = left


data NameCases (a :: ObjType) = NameCases { _inRoomDesc :: Maybe (ObjRef a InRoomDesc)
                                          , _nominative :: Maybe (ObjRef a Nominative)
                                          , _genitive :: Maybe (ObjRef a Genitive)
                                          , _accusative :: Maybe (ObjRef a Accusative)
                                          , _dative :: Maybe (ObjRef a Dative)
                                          , _instrumental :: Maybe (ObjRef a Instrumental)
                                          , _prepositional :: Maybe (ObjRef a Prepositional)
                                          , _alias :: Maybe (ObjRef a Alias)
                                          } deriving (Eq, Ord, Show, Generic)

instance Semigroup (NameCases a) where
  left <> right = NameCases { _inRoomDesc = _inRoomDesc left <> _inRoomDesc right
                            , _nominative = _nominative left <> _nominative right
                            , _genitive = _genitive left <> _genitive right
                            , _accusative = _accusative left <> _accusative right
                            , _dative = _dative left <> _dative right
                            , _instrumental = _instrumental left <> _instrumental right
                            , _prepositional = _prepositional left <> _prepositional right
                            , _alias = _alias left <> _alias right
                            }

instance Monoid (NameCases a) where
  mempty = NameCases { _inRoomDesc = Nothing
                     , _nominative = Nothing
                     , _genitive = Nothing
                     , _accusative = Nothing
                     , _dative = Nothing
                     , _instrumental = Nothing
                     , _prepositional = Nothing
                     , _alias = Nothing
                     }

newtype EverAttacked = EverAttacked Bool deriving (Eq, Ord, Show)

instance Semigroup EverAttacked where
  (EverAttacked left) <> (EverAttacked right) = EverAttacked $ left || right

data MobStats = MobStats { _nameCases :: NameCases Mob
                         , _everAttacked :: Maybe (EverAttacked)
                         } deriving (Eq, Ord, Generic, Show)

instance TextShow MobStats where
  showt = showt . _nameCases

instance TextShow (NameCases Mob) where
  showt v = 
    T.intercalate "\n" $
     [ maybe "" unObjRef $ _inRoomDesc v
     , maybe "" unObjRef $ _nominative v
     , maybe "" unObjRef $ _genitive v
     , maybe "" unObjRef $ _accusative v
     , maybe "" unObjRef $ _dative v
     , maybe "" unObjRef $ _instrumental v
     , maybe "" unObjRef $ _prepositional v
     ]
  --showt v = T.intercalate "\n" . ((\f -> (maybe "" showt . unObjRef . f) v) <$> [_inRoomDesc, _nominative, _genitive, _accusative, _dative, _instrumental, _prepositional])

instance Semigroup MobStats where
  left <> right = MobStats { _nameCases = _nameCases left <> _nameCases right
                           , _everAttacked = _everAttacked left <> _everAttacked right
                           }

instance Monoid MobStats where
  mempty = MobStats { _nameCases = mempty
                    , _everAttacked = mempty
                    }

type MobRoomDesc = ObjRef Mob InRoomDesc

instance TextShow (ObjRef a b) where
  showt = unObjRef

data RoomDir = North | South | East | West | Up | Down deriving (Eq, Generic, Ord, Show)
data RoomExit = OpenExit RoomDir | ClosedExit RoomDir deriving (Eq, Generic, Ord, Show)

class ShowVal a where
  showVal :: a -> Text

instance ShowVal LocationId where
  showVal (LocationId id) = show id

instance ShowVal LocationTitle where
  showVal (LocationTitle text) = text


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

makeLenses ''MobStats
makeLenses ''NameCases

makePrisms ''ServerEvent
makePrisms ''Event
