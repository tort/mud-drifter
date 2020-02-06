{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ServerInputParserSpec (spec) where

import Protolude hiding (Location)
import qualified Protolude as P
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.ByteString.Char8 as C8
import Test.Hspec.Attoparsec
import qualified Pipes.ByteString as PBS
import Pipes hiding ((~>))
import Pipes.Prelude hiding (fromHandle, filter, length, mapM_, print)
import qualified Pipes.Prelude as PP
import qualified Data.Text as T
import TextShow
import Data.Text.Encoding
import Data.String
import Control.Monad
import qualified Data.List as L

import Person
import ServerInputParser
import RemoteConsole
import Event
import World

spec :: Spec
spec = describe "Parser" $ do
        it "does not fail on input" $ do log <- C8.readFile "test/logs/common.log"
                                         shouldSucceedOn serverInputParser log
        it "parse codepage prompt" $ do log <- C8.readFile "test/logs/codepagePrompt.log"
                                        log ~> serverInputParser `shouldParse` CodepagePrompt
        it "parse login prompt" $ do log <- C8.readFile "test/logs/loginPrompt.log"
                                     log ~> serverInputParser `shouldParse` LoginPrompt
        it "parse password prompt" $ do log <- C8.readFile "test/logs/passwordPrompt.log"
                                        log ~> serverInputParser `shouldParse` PasswordPrompt
        it "parse welcome prompt" $ do log <- C8.readFile "test/logs/welcomePrompt.log"
                                       log ~> serverInputParser `shouldParse` WelcomePrompt
        it "parse post welcome message" $ do log <- C8.readFile "test/logs/postWelcome.log"
                                             log ~> serverInputParser `shouldParse` PostWelcome
        it "parse location" $ do log <- C8.readFile "test/logs/locationMessage.log"
                                 log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5032) (LocationTitle "В светлой комнате")
                                                                                       , _objects = [ ItemRoomDesc "Ваш походный сундучок стоит здесь." ]
                                                                                       , _mobs = [ MobRoomDesc "Дочка старейшины стоит здесь."
                                                                                                 , MobRoomDesc "Дородная женщина стоит здесь." ]
                                                                                       }
        it "parse location with closed doors" $ do log <- C8.readFile "test/logs/locationWithClosedDoor.log"
                                                   log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5102) (LocationTitle "В сенях")
                                                                                                        , _objects = [ ]
                                                                                                        , _mobs = [ MobRoomDesc "Клоп ползает здесь." ]
                                                                                                        }
        it "trims mobs room descriptions" $ do log <- C8.readFile "test/logs/locationWithAutoExits.log"
                                               log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5000) (LocationTitle "Комнаты отдыха")
                                                                                                    , _objects = [ ItemRoomDesc "У ваших ног лежит глиняная плошка."
                                                                                                                 , ItemRoomDesc "Доска для различных заметок и объявлений прибита тут ..блестит!"
                                                                                                                 ]
                                                                                                    , _mobs = [ MobRoomDesc "Полянин Дорман стоит здесь."
                                                                                                              , MobRoomDesc "Хозяйка постоялого двора распоряжается здесь."
                                                                                                              ]
                                                                                                    }
        it "parse location with autoexits" $ do log <- C8.readFile "test/logs/locationWithAutoExits.log"
                                                log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5000) (LocationTitle "Комнаты отдыха")
                                                                                                     , _objects = [ ItemRoomDesc "У ваших ног лежит глиняная плошка."
                                                                                                                  , ItemRoomDesc "Доска для различных заметок и объявлений прибита тут ..блестит!"
                                                                                                                  ]
                                                                                                     , _mobs = [ MobRoomDesc "Полянин Дорман стоит здесь."
                                                                                                               , MobRoomDesc "Хозяйка постоялого двора распоряжается здесь."
                                                                                                               ]
                                                                                                     }
        it "parse move to location" $ do log <- C8.readFile "test/logs/move.log"
                                         log ~> serverInputParser `shouldParse` (MoveEvent "юг")
        it "parse move in darkness with nightvision" $ do log <- C8.readFile "test/logs/inDarknessWithInfra.log"
                                                          log ~> serverInputParser `shouldParse` (MoveEvent "север")
        it "parse in darkness server event" $ do let log = "test/logs/enterDarkRoom.log"
                                                 serverEventList <- toListM $ loadAndParseServerEvents log >-> PP.filter nonEmptyUnknown
                                                 P.take 2 serverEventList `shouldBe` [MoveEvent "вниз", DarknessEvent]
        it "parse log, starting from partial move message" $ do let simpleWalkFile = "test/logs/startingInTheMiddleOfMove.log"
                                                                (locationEventsCount, moveEventsCount) <- locationsAndCounts simpleWalkFile
                                                                (expectedLocationsCount, expectedMoveCount) <- expectedLocsAndMovesCounts simpleWalkFile
                                                                moveEventsCount `shouldBe` expectedMoveCount
                                                                locationEventsCount `shouldBe` expectedLocationsCount
        it "parse log, finishing on partial move message" $ do let simpleWalkFile = "test/logs/finishingInTheMiddleOfMove.log"
                                                               (locationEventsCount, moveEventsCount) <- locationsAndCounts simpleWalkFile
                                                               (expectedLocationsCount, expectedMoveCount) <- expectedLocsAndMovesCounts simpleWalkFile
                                                               moveEventsCount `shouldBe` expectedMoveCount
                                                               locationEventsCount `shouldBe` (expectedLocationsCount - 1)
        it "parse multiple moves" $ do let simpleWalkFile = "test/logs/simpleWalk.log"
                                       (locationEventsCount, moveEventsCount) <- locationsAndCounts simpleWalkFile
                                       (expectedLocationsCount, expectedMoveCount) <- expectedLocsAndMovesCounts simpleWalkFile
                                       moveEventsCount `shouldBe` expectedMoveCount
                                       locationEventsCount `shouldBe` expectedLocationsCount
        it "parse multiple messages in one GA frame" $ do let log = "test/logs/multipleEventsInGA.log"
                                                          serverEventList <- toListM $ loadAndParseServerEvents log >-> PP.filter nonEmptyUnknown
                                                          length serverEventList `shouldBe` 5
        it "parse sample trigger text" $ do let log = "test/logs/triggerText.log"
                                            serverEventList <- toListM $ parseServerEvents (loadServerEvents log) >-> PP.filter nonEmptyUnknown
                                            length serverEventList `shouldBe` 6
        it "parse move and location on agromob" $ do let log = "test/logs/enterRoomWithFight2.log"
                                                     serverEventList <- toListM $ parseServerEvents (loadServerEvents log) >-> PP.filter moveOrLocation
                                                     serverEventList `shouldBe` [ MoveEvent "восток"
                                                                                , LocationEvent { _location = (Location (LocationId 5112) (LocationTitle "На кухне"))
                                                                                                , _objects = []
                                                                                                , _mobs = [ MobRoomDesc "(летит) Комар жужжит здесь."
                                                                                                         , MobRoomDesc "Таракан быстро пробежал здесь."
                                                                                                         , MobRoomDesc "Блоха прячется в мусоре."
                                                                                                         , MobRoomDesc "(летит) Моль летает здесь."
                                                                                                         ]
                                                                                                }
                                                                                ]
        it "parse equipment list" $ do log <- C8.readFile "test/logs/listEquipment2.log"
                                       log ~> serverInputParser `shouldParse` (ListEquipmentEvent [ (EquippedItem Body (ItemNominative "легкий латный доспех"), Excellent)
                                                                                                  , (EquippedItem Head (ItemNominative "легкий латный шлем"), Excellent)
                                                                                                  , (EquippedItem Legs (ItemNominative "легкие латные поножи"), Excellent)
                                                                                                  , (EquippedItem Waist (ItemNominative "холщовый мешок"), Excellent)
                                                                                                  , (EquippedItem Wield (ItemNominative "длинный бронзовый меч"), VeryGood)
                                                                                                  , (EquippedItem Hold (ItemNominative "бронзовый топорик"), VeryGood)
                                                                                                  ])
        it "parse empty equipment list" $ do log <- C8.readFile "test/logs/listEquipmentEmpty.log"
                                             log ~> serverInputParser `shouldParse` (ListEquipmentEvent [])
        it "parse inventory" $ do log <- C8.readFile "test/logs/inventory.log"
                                  log ~> serverInputParser `shouldParse` (ListInventoryEvent [ (ItemNominative "холщовый мешок", Excellent)
                                                                                             , (ItemNominative "бронзовый топорик", VeryGood)
                                                                                             , (ItemNominative "длинный бронзовый меч", VeryGood)
                                                                                             ])
        it "parse cr after unknown server event" $ do let log = "test/logs/mobPortal.log"
                                                      serverEventList <- toListM $ parseServerEvents (loadServerEvents log)
                                                      (length $ filter isLocationEvent serverEventList) `shouldBe` 3
                                                      (length $ filter isMoveEvent serverEventList) `shouldBe` 1
        it "parse empty inventory" $ do log <- C8.readFile "test/logs/inventoryEmpty.log"
                                        log ~> serverInputParser `shouldParse` (ListInventoryEvent [])
        it "parse weapon stats in shop" $ do log <- C8.readFile "test/logs/statsWeapon.log"
                                             log ~> serverInputParser `shouldParse` (ItemStatsEvent $ Weapon (ItemNominative "длинный бронзовый меч") LongBlade [Wield, Hold, DualWield] 3.5)
        it "parse armor stats in shop" $ do log <- C8.readFile "test/logs/statsArmor.log"
                                            log ~> serverInputParser `shouldParse` (ItemStatsEvent $ Armor (ItemNominative "легкий латный доспех") [Body] 3 4)
        it "parse weapon stats" $ do log <- C8.readFile "test/logs/statsWeaponScroll.log"
                                     log ~> serverInputParser `shouldParse` (ItemStatsEvent $ Weapon (ItemNominative "бронзовый топорик") Axe [Wield, Hold] 3.5)
        it "parse shop items" $ do let log = "test/logs/shopList.log"
                                   serverEventList <- toListM $ parseServerEvents (loadServerEvents log) >-> PP.filter isShopListItemEvent
                                   length serverEventList `shouldBe` 43
        it "parse shop list with prompt" $ do let log = "test/logs/shopListWithPrompt.log"
                                              serverEventList <- toListM $ parseServerEvents $ loadServerEvents log
                                              (length $ filter isShopListItemEvent serverEventList) `shouldBe` 27
                                              (length $ filter (== PromptEvent) serverEventList) `shouldBe` 1
        it "parse single line prompt event" $ do log <- C8.readFile "test/logs/prompt.1.log"
                                                 log ~> serverInputParser `shouldParse` PromptEvent
        it "parse two-line prompt event" $ do log <- C8.readFile "test/logs/prompt.2.log"
                                              log ~> serverInputParser `shouldParse` PromptEvent
        it "parse fight prompt" $ do let log = "test/logs/enterRoomWithFight.log"
                                     serverEventList <- toListM $ parseServerEvents $ loadServerEvents log
                                     (length $ filter (== FightPromptEvent) serverEventList) `shouldBe` 3
        it "parse school entrance location" $ let location = Location (LocationId 5000) (LocationTitle "Комнаты отдыха")
                                                  objects = [ItemRoomDesc "Доска для различных заметок и объявлений прибита тут ..блестит!"]
                                                  mobs = [ MobRoomDesc "Полянин Дорман стоит здесь."
                                                         , MobRoomDesc "Хозяйка постоялого двора распоряжается здесь."
                                                         ]
                                               in do log <- C8.readFile "test/logs/schoolEntrance.log"
                                                     log ~> serverInputParser `shouldParse` (LocationEvent location objects mobs)
        it "parse unknown obstacle when glancing to direction" $ do log <- C8.readFile "test/logs/openDoor.1.log"
                                                                    log ~> serverInputParser `shouldParse` (ObstacleEvent South "дверь")
        it "parse known obstacle when glancing to direction" $ do log <- C8.readFile "test/logs/openDoor.2.log"
                                                                  log ~> serverInputParser `shouldParse` (ObstacleEvent North "ворота")
        it "parse failure to go in direction" $ do log <- C8.readFile "test/logs/noWayThisDir.log"
                                                   log ~> serverInputParser `shouldParse` CantGoDir
        it "parse zone border exits" $ do log <- C8.readFile "test/logs/zoneBorderExit.log"
                                          log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5023) (LocationTitle "Заброшенный дом")
                                                                                               , _objects = []
                                                                                               , _mobs = [ MobRoomDesc "Местная жительница идет по своим делам."
                                                                                                         , MobRoomDesc "Местный житель идет здесь."
                                                                                                         ]
                                                                                               }
        it "parse objects in the room" $ do log <- C8.readFile "test/logs/roomWithObjects.log"
                                            log ~> serverInputParser `shouldParse` (LocationEvent location objects mobs)
                                              where objects = [ ItemRoomDesc "Лужица дождевой воды разлита у ваших ног." ]
                                                    location = Location (LocationId 5026) (LocationTitle "Лесная улица")
                                                    mobs = [MobRoomDesc "Пожилой широкоплечий крестьянин в добротной одежде прохаживается тут."]

instance TextShow ServerEvent where
  showt (LocationEvent loc items mobs) = renderTitle <> "\n  Items: " <> renderItems <> "\n  Mobs: " <> renderMobs
    where renderTitle = showt loc
          renderItems = T.intercalate "\n  " $ fmap renderItem items
          renderItem (ItemRoomDesc text) = text
          renderMobs = T.intercalate "\n  " $ fmap renderMob mobs
          renderMob (MobRoomDesc text) = text
  showt _ = ""

moveOrLocation :: ServerEvent -> Bool
moveOrLocation e = isMoveEvent e || isLocationEvent e

nonEmptyUnknown :: ServerEvent -> Bool
nonEmptyUnknown (UnknownServerEvent "") = False
nonEmptyUnknown _ = True

locationsAndCounts :: FilePath -> IO (Int, Int)
locationsAndCounts file = do
  serverEventList <- PP.toListM $ parseServerEvents $ loadServerEvents file
  let locationEventsCount = length (filter isLocationEvent serverEventList)
  let moveEventsCount = length (filter isMoveEvent serverEventList)
  return (locationEventsCount, moveEventsCount)

expectedLocsAndMovesCounts :: String -> IO (Int, Int)
expectedLocsAndMovesCounts file = do
  lines <- C8.lines <$> C8.readFile file
  let expectedLocationsCount = countStringsWith (\s -> C8.isInfixOf "1;36m" s && C8.isInfixOf "[" s && C8.isInfixOf "]" s)
      expectedMoveCount = countStringsWith (C8.isInfixOf $ encodeUtf8 "Вы поплелись")
      countStringsWith predicate = length . (filter predicate) $ lines
  return (expectedLocationsCount, expectedMoveCount)

loadAndParseServerEvents :: FilePath -> Producer ServerEvent IO ()
loadAndParseServerEvents = parseServerEvents . loadServerEvents
