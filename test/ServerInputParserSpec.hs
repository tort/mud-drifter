module ServerInputParserSpec (spec) where

import Protolude hiding (Location, Up, Down)
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
import Control.Lens
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
  {-
	it "parse login prompt" $ do log <- C8.readFile "test/logs/loginPrompt.log"
                                     log ~> serverInputParser `shouldParse` LoginPrompt
-}
        it "parse password prompt" $ do log <- C8.readFile "test/logs/passwordPrompt.log"
                                        log ~> serverInputParser `shouldParse` PasswordPrompt
        it "parse welcome prompt" $ do log <- C8.readFile "test/logs/welcomePrompt.log"
                                       log ~> serverInputParser `shouldParse` WelcomePrompt
        it "parse post welcome message" $ do log <- C8.readFile "test/logs/postWelcome.log"
                                             log ~> serverInputParser `shouldParse` PostWelcome
        it "parse location" $ do log <- C8.readFile "test/logs/locationMessage.log"
                                 log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5032) (LocationTitle "В светлой комнате")
                                                                                       , _objects = [ ObjRef "Ваш походный сундучок стоит здесь." ]
                                                                                       , _mobs = [ ObjRef "Дочка старейшины стоит здесь."
                                                                                                 , ObjRef "Дородная женщина стоит здесь." ]
                                                                                      , _exits = [OpenExit North]
                                                                                       }
        it "parse location with closed doors" $ do log <- C8.readFile "test/logs/locationWithClosedDoor.log"
                                                   log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5102) (LocationTitle "В сенях")
                                                                                                        , _objects = [ ]
                                                                                                        , _mobs = [ ObjRef "Клоп ползает здесь." ]
                                                                                                        , _exits = [ClosedExit North,OpenExit East,ClosedExit South]
                                                                                                        }
        it "trims mobs room descriptions" $ do log <- C8.readFile "test/logs/locationWithAutoExits.log"
                                               log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5000) (LocationTitle "Комнаты отдыха")
                                                                                                    , _objects = [ ObjRef "У ваших ног лежит глиняная плошка."
                                                                                                                 , ObjRef "Доска для различных заметок и объявлений прибита тут ..блестит!"
                                                                                                                 ]
                                                                                                    , _mobs = [ ObjRef "Полянин Дорман стоит здесь."
                                                                                                              , ObjRef "Хозяйка постоялого двора распоряжается здесь."
                                                                                                              ]
                                                                                                    , _exits = [OpenExit Down]
                                                                                                    }
        it "parse location with autoexits" $ do log <- C8.readFile "test/logs/locationWithAutoExits.log"
                                                log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5000) (LocationTitle "Комнаты отдыха")
                                                                                                     , _objects = [ ObjRef "У ваших ног лежит глиняная плошка."
                                                                                                                  , ObjRef "Доска для различных заметок и объявлений прибита тут ..блестит!"
                                                                                                                  ]
                                                                                                     , _mobs = [ ObjRef "Полянин Дорман стоит здесь."
                                                                                                               , ObjRef "Хозяйка постоялого двора распоряжается здесь."
                                                                                                               ]
                                                                                                     , _exits = [OpenExit Down]
                                                                                                     }
        it "parse move to location" $ do log <- C8.readFile "test/logs/move.log"
                                         log ~> serverInputParser `shouldParse` (MoveEvent "юг")
        it "parse location with thin ice" $ do log <- C8.readFile "test/logs/locationWithThinIce.log"
                                               log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5600) (LocationTitle "У истока реки")
                                                                                                    , _objects = []
                                                                                                    , _mobs = []
                                                                                                    , _exits = [OpenExit North, OpenExit South]
                                                                                                    }
        it "parse location with ice" $ do log <- C8.readFile "test/logs/locationWithIce.log"
                                          log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5601) (LocationTitle "Мелководье")
                                                                                               , _objects = [ ObjRef "Лужица ржаного кваса разлита у ваших ног." ]
                                                                                               , _mobs = []
                                                                                               , _exits = [OpenExit North, OpenExit South]
                                                                                               }
        it "parse location with mud" $ do log <- C8.readFile "test/logs/muddyLocation.log"
                                          log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5200) (LocationTitle "Лесная дорога")
                                                                                               , _objects = []
                                                                                               , _mobs = []
                                                                                               , _exits = [OpenExit North, OpenExit East, OpenExit South, OpenExit West]
                                                                                               }
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
        it "parse mob entered the room" $ do let log = "test/logs/mob-enters-the-room.log"
                                             serverEventList <- toListM $ loadAndParseServerEvents log >-> PP.filter (has (_MobWentIn))
                                             length serverEventList `shouldBe` 2
        it "parse sample trigger text" $ do let log = "test/logs/triggerText.log"
                                            serverEventList <- toListM $ parseServerEvents (loadServerEvents log) >-> PP.filter nonEmptyUnknown
                                            length serverEventList `shouldBe` 6
        it "parse move and location on agromob" $ do let log = "test/logs/enterRoomWithFight2.log"
                                                     serverEventList <- toListM $ parseServerEvents (loadServerEvents log) >-> PP.filter moveOrLocation
                                                     serverEventList `shouldBe` [ MoveEvent "восток"
                                                                                , LocationEvent { _location = (Location (LocationId 5112) (LocationTitle "На кухне"))
                                                                                                , _objects = []
                                                                                                , _mobs = [ ObjRef "(летит) Комар жужжит здесь."
                                                                                                         , ObjRef "Таракан быстро пробежал здесь."
                                                                                                         , ObjRef "Блоха прячется в мусоре."
                                                                                                         , ObjRef "(летит) Моль летает здесь."
                                                                                                         ]
                                                                                                , _exits = [OpenExit North,OpenExit West,OpenExit Down]
                                                                                                }
                                                                                ]
        it "parse equipment list" $ do log <- C8.readFile "test/logs/listEquipment2.log"
                                       log ~> serverInputParser `shouldParse` (ListEquipmentEvent [ (EquippedItem Body (ObjRef "легкий латный доспех"), Excellent)
                                                                                                  , (EquippedItem Head (ObjRef "легкий латный шлем"), Excellent)
                                                                                                  , (EquippedItem Legs (ObjRef "легкие латные поножи"), Excellent)
                                                                                                  , (EquippedItem Waist (ObjRef "холщовый мешок"), Excellent)
                                                                                                  , (EquippedItem Wield (ObjRef "длинный бронзовый меч"), VeryGood)
                                                                                                  , (EquippedItem Hold (ObjRef "бронзовый топорик"), VeryGood)
                                                                                                  ])
        it "parse empty equipment list" $ do log <- C8.readFile "test/logs/listEquipmentEmpty.log"
                                             log ~> serverInputParser `shouldParse` (ListEquipmentEvent [])
        it "parse inventory" $ do log <- C8.readFile "test/logs/inventory.log"
                                  log ~> serverInputParser `shouldParse` (ListInventoryEvent [ (ObjRef "холщовый мешок", Excellent)
                                                                                             , (ObjRef "бронзовый топорик", VeryGood)
                                                                                             , (ObjRef "длинный бронзовый меч", VeryGood)
                                                                                             ])
        it "parse cr after unknown server event" $ do let log = "test/logs/mobPortal.log"
                                                      serverEventList <- toListM $ parseServerEvents (loadServerEvents log)
                                                      (length $ filter (has _LocationEvent) serverEventList) `shouldBe` 3
                                                      (length $ filter (has _MoveEvent) serverEventList) `shouldBe` 1
        it "parse empty inventory" $ do log <- C8.readFile "test/logs/inventoryEmpty.log"
                                        log ~> serverInputParser `shouldParse` (ListInventoryEvent [])
        it "parse weapon stats in shop" $ do log <- C8.readFile "test/logs/statsWeapon.log"
                                             log ~> serverInputParser `shouldParse` (ItemStatsEvent $ Weapon (ObjRef "длинный бронзовый меч") LongBlade [Wield, Hold, DualWield] 3.5)
        it "parse armor stats in shop" $ do log <- C8.readFile "test/logs/statsArmor.log"
                                            log ~> serverInputParser `shouldParse` (ItemStatsEvent $ Armor (ObjRef "легкий латный доспех") [Body] 3 4)
        it "parse weapon stats" $ do log <- C8.readFile "test/logs/statsWeaponScroll.log"
                                     log ~> serverInputParser `shouldParse` (ItemStatsEvent $ Weapon (ObjRef "бронзовый топорик") Axe [Wield, Hold] 3.5)
        it "parse shop items" $ do let log = "test/logs/shopList.log"
                                   serverEventList <- toListM $ parseServerEvents (loadServerEvents log) >-> PP.filter (has _ShopListItemEvent)
                                   length serverEventList `shouldBe` 43
        it "parse examine container event" $ do log <- C8.readFile "test/logs/examineContainer.log"
                                                log ~> serverInputParser `shouldParse` ExamineContainer { _name = "холщовый мешок"
                                                                                                        , _items = [ Single (ObjRef "рыбья кость") Excellent
                                                                                                                   , Multiple (ObjRef "ломоть хлеба") 11
                                                                                                                   ]
                                                                                                        }
        it "parse shop list with prompt" $ do let log = "test/logs/shopListWithPrompt.log"
                                              serverEventList <- toListM $ parseServerEvents $ loadServerEvents log
                                              (length $ filter (has _ShopListItemEvent) serverEventList) `shouldBe` 27
                                              (length $ filter (has _PromptEvent) serverEventList) `shouldBe` 1
        it "parse two-line prompt event" $ do log <- C8.readFile "test/logs/prompt.2.log"
                                              log ~> serverInputParser `shouldParse` PromptEvent 143 101
        it "parse school entrance location" $ let location = Location (LocationId 5000) (LocationTitle "Комнаты отдыха")
                                                  objects = [ObjRef "Доска для различных заметок и объявлений прибита тут ..блестит!"]
                                                  mobs = [ ObjRef "Полянин Дорман стоит здесь."
                                                         , ObjRef "Хозяйка постоялого двора распоряжается здесь."
                                                         ]
                                                  exits = [OpenExit Down]
                                               in do log <- C8.readFile "test/logs/schoolEntrance.log"
                                                     log ~> serverInputParser `shouldParse` (LocationEvent location objects mobs exits)
        it "parse school entrance location misspelled" $ do
          log <- C8.readFile "test/logs/rentLocation.log"
          log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 4056) (LocationTitle "Гостиный двор")
                                                               , _objects = [ ObjRef "Доска для различных заметок и объявлений прибита тут ..блестит!" ]
                                                               , _mobs = [ ObjRef "Велянка Ванесса летает здесь."
                                                                         , ObjRef "Шум и блеск экипировки выдает чье-то присутствие."
                                                                         , ObjRef "С орлиным клювом пересмешник Пересмех (пономарь Тайных знаний) летает здесь."
                                                                         , ObjRef "Хозяин постоялого двора с интересом рассматривает Вас."
                                                                         ]
                                                               , _exits = [ OpenExit East ]
                                                               }
        it "parse unknown obstacle when glancing to direction" $ do log <- C8.readFile "test/logs/openDoor.1.log"
                                                                    log ~> serverInputParser `shouldParse` (ObstacleEvent South "дверь")
        it "parse known obstacle when glancing to direction" $ do log <- C8.readFile "test/logs/openDoor.2.log"
                                                                  log ~> serverInputParser `shouldParse` (ObstacleEvent North "ворота")
        it "parse failure to go in direction" $ do log <- C8.readFile "test/logs/noWayThisDir.log"
                                                   log ~> serverInputParser `shouldParse` CantGoDir
        it "parse zone border exits" $ do log <- C8.readFile "test/logs/zoneBorderExit.log"
                                          log ~> serverInputParser `shouldParse` LocationEvent { _location = Location (LocationId 5023) (LocationTitle "Заброшенный дом")
                                                                                               , _objects = []
                                                                                               , _mobs = [ ObjRef "Местная жительница идет по своим делам."
                                                                                                         , ObjRef "Местный житель идет здесь."
                                                                                                         ]
                                                                                               , _exits = [OpenExit East, OpenExit West]
                                                                                               }
        it "parses my stats" $ do log <- C8.readFile "test/logs/myStats.log"
                                  log ~> serverInputParser `shouldParse` (MyStats 223 112)
        it "parses loot message" $ do log <- C8.readFile "test/logs/loot.log"
                                      log ~> serverInputParser `shouldParse` (LootItem (ObjRef "грабли") (ObjRef "огородника"))
        it "parses one coin loot" $ do log <- C8.readFile "test/logs/oneCoinLoot.log"
                                       log ~> serverInputParser `shouldParse` LootMoney (ObjRef "чибиса")
        it "parses one pile of coins loot" $ do log <- C8.readFile "test/logs/pileOfCoinsLoot.log"
                                                log ~> serverInputParser `shouldParse` LootMoney (ObjRef "огородника")
        it "parses i'm bashed" $ do log <- C8.readFile "test/logs/imBashed.log"
                                    log ~> serverInputParser `shouldParse` ImBashedEvent
        it "parses i'm bashed with colors drop" $ do log <- C8.readFile "test/logs/imBashed.2.log"
                                                     log ~> serverInputParser `shouldParse` ImBashedEvent
        it "parse objects in the room" $ do log <- C8.readFile "test/logs/roomWithObjects.log"
                                            log ~> serverInputParser `shouldParse` (LocationEvent location objects mobs exits)
                                              where objects = [ ObjRef "Лужица дождевой воды разлита у ваших ног." ]
                                                    location = Location (LocationId 5026) (LocationTitle "Лесная улица")
                                                    mobs = [ObjRef "Пожилой широкоплечий крестьянин в добротной одежде прохаживается тут."]
                                                    exits = [OpenExit North,OpenExit South]


moveOrLocation :: ServerEvent -> Bool
moveOrLocation e = has _MoveEvent e || has _LocationEvent e

nonEmptyUnknown :: ServerEvent -> Bool
nonEmptyUnknown (UnknownServerEvent "") = False
nonEmptyUnknown _ = True

locationsAndCounts :: FilePath -> IO (Int, Int)
locationsAndCounts file = do
  serverEventList <- PP.toListM $ parseServerEvents $ loadServerEvents file
  let locationEventsCount = length (filter (has _LocationEvent) serverEventList)
  let moveEventsCount = length (filter (has _MoveEvent) serverEventList)
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
