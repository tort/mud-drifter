{-# LANGUAGE OverloadedStrings #-}

module ServerInputParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.ByteString.Char8 hiding (filter, length)
import Test.Hspec.Attoparsec
import Pipes.ByteString hiding (filter, length, lines)
import Prelude hiding (readFile, putStrLn, lines)
import qualified Prelude as P
import System.IO hiding (readFile, putStrLn, hGetContents)
import Pipes hiding ((~>))
import Pipes.Prelude hiding (fromHandle, filter, length, mapM_, print)
import qualified Pipes.Prelude as PP
import Data.Text hiding (isInfixOf, isPrefixOf, length, filter, lines)
import Data.Text.Encoding
import Control.Monad

import ServerInputParser
import Person
import RemoteConsole
import Event

spec :: Spec
spec = describe "Parser" $ do
        it "does not fail on input" $ do log <- readFile "test/logs/common.log"
                                         shouldSucceedOn serverInputParser log
        it "parse codepage prompt" $ do log <- readFile "test/logs/codepagePrompt.log"
                                        log ~> serverInputParser `shouldParse` CodepagePrompt
        it "parse login prompt" $ do log <- readFile "test/logs/loginPrompt.log"
                                     log ~> serverInputParser `shouldParse` LoginPrompt
        it "parse password prompt" $ do log <- readFile "test/logs/passwordPrompt.log"
                                        log ~> serverInputParser `shouldParse` PasswordPrompt
        it "parse welcome prompt" $ do log <- readFile "test/logs/welcomePrompt.log"
                                       log ~> serverInputParser `shouldParse` WelcomePrompt
        it "parse post welcome message" $ do log <- readFile "test/logs/postWelcome.log"
                                             log ~> serverInputParser `shouldParse` PostWelcome
        it "parse location" $ do log <- readFile "test/logs/locationMessage.log"
                                 log ~> serverInputParser `shouldParse` (LocationEvent (Location 35040 "В корчме") [])
        it "parse move to location" $ do log <- readFile "test/logs/move.log"
                                         log ~> serverInputParser `shouldParse` (MoveEvent "юг")
        it "parse move in darkness with nightvision" $ do log <- readFile "test/logs/inDarknessWithInfra.log"
                                                          log ~> serverInputParser `shouldParse` (MoveEvent "север")
        it "parse in darkness server event" $ do hLog <- openFile "test/logs/enterDarkRoom.log" ReadMode
                                                 serverEventList <- toListM $ parseProducer (fromHandle hLog) >-> toJustRight >-> PP.filter nonEmptyUnknown
                                                 P.take 2 serverEventList `shouldBe` [MoveEvent "вниз", DarknessEvent]
                                                 hClose hLog
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
        it "parse multiple messages in one GA frame" $ do hLog <- openFile "test/logs/multipleEventsInGA.log" ReadMode
                                                          serverEventList <- toListM $ parseProducer (fromHandle hLog) >-> toJustRight >-> PP.filter nonEmptyUnknown
                                                          length serverEventList `shouldBe` 5
                                                          hClose hLog
        it "parse sample trigger text" $ do hLog <- openFile "test/logs/triggerText.log" ReadMode
                                            serverEventList <- toListM $ parseProducer (fromHandle hLog) >-> toJustRight >-> PP.filter nonEmptyUnknown
                                            length serverEventList `shouldBe` 6
                                            hClose hLog
        it "parse move and location on agromob" $ do hLog <- openFile "test/logs/enterRoomWithFight.log" ReadMode
                                                     serverEventList <- toListM $ parseProducer (fromHandle hLog) >-> toJustRight >-> PP.filter moveOrLocation
                                                     serverEventList `shouldBe` [MoveEvent "восток", (LocationEvent (Location 5112 "На кухне") [])]
                                                     hClose hLog
        it "parse equipment list" $ do log <- readFile "test/logs/listEquipment2.log"
                                       log ~> serverInputParser `shouldParse` (ListEquipmentEvent [ (EquippedItem Body "легкий латный доспех", Excellent)
                                                                                                  , (EquippedItem Head "легкий латный шлем", Excellent)
                                                                                                  , (EquippedItem Legs "легкие латные поножи", Excellent)
                                                                                                  , (EquippedItem Waist "холщовый мешок", Excellent)
                                                                                                  , (EquippedItem Wield "длинный бронзовый меч", VeryGood)
                                                                                                  , (EquippedItem Hold "бронзовый топорик", VeryGood)
                                                                                                  ])
        it "parse empty equipment list" $ do log <- readFile "test/logs/listEquipmentEmpty.log"
                                             log ~> serverInputParser `shouldParse` (ListEquipmentEvent [])
        it "parse inventory" $ do log <- readFile "test/logs/inventory.log"
                                  log ~> serverInputParser `shouldParse` (ListInventoryEvent [ ("холщовый мешок", Excellent)
                                                                                             , ("бронзовый топорик", VeryGood)
                                                                                             , ("длинный бронзовый меч", VeryGood)
                                                                                             ])
        it "parse empty inventory" $ do log <- readFile "test/logs/inventoryEmpty.log"
                                        log ~> serverInputParser `shouldParse` (ListInventoryEvent [])
        it "parse weapon stats in shop" $ do log <- readFile "test/logs/statsWeapon.log"
                                             log ~> serverInputParser `shouldParse` (ItemStatsEvent $ Weapon "длинный бронзовый меч" LongBlade [Wield, Hold, DualWield] 3.5)
        it "parse armor stats in shop" $ do log <- readFile "test/logs/statsArmor.log"
                                            log ~> serverInputParser `shouldParse` (ItemStatsEvent $ Armor "легкий латный доспех" [Body] 3 4)
        it "parse weapon stats" $ do log <- readFile "test/logs/statsWeaponScroll.log"
                                     log ~> serverInputParser `shouldParse` (ItemStatsEvent $ Weapon "бронзовый топорик" Axe [Wield, Hold] 3.5)
        it "parse shop items" $ do hLog <- openFile "test/logs/shopList.log" ReadMode
                                   serverEventList <- toListM $ parseProducer (fromHandle hLog) >-> toJustRight >-> PP.filter isShopListIemEvent
                                   length serverEventList `shouldBe` 43
                                   hClose hLog
        it "parse single line prompt event" $ do log <- readFile "test/logs/prompt.1.log"
                                                 log ~> serverInputParser `shouldParse` PromptEvent
        it "parse two-line prompt event" $ do log <- readFile "test/logs/prompt.2.log"
                                              log ~> serverInputParser `shouldParse` PromptEvent
        it "parse objects in the room" $ do log <- readFile "test/logs/roomWithObjects.2.log"
                                            log ~> serverInputParser `shouldParse` (LocationEvent location objects)
                                              where objects = ["Неочищенная руда лежит у Вас под ногами.", "На полу лежит лестница."]
                                                    location = Location 5104 "На сеновале"

isShopListIemEvent :: ServerEvent -> Bool
isShopListIemEvent (ShopListItemEvent _ _) = True
isShopListIemEvent _ = False

moveOrLocation :: ServerEvent -> Bool
moveOrLocation (MoveEvent _) = True
moveOrLocation (LocationEvent _ _) = True
moveOrLocation _ = False

nonEmptyUnknown :: ServerEvent -> Bool
nonEmptyUnknown (UnknownServerEvent "") = False
nonEmptyUnknown _ = True

toJustRight :: Pipe (Maybe (Either a ServerEvent)) ServerEvent IO ()
toJustRight = forever $ do e <- await
                           act e
                             where act (Just (Right evt)) = yield evt
                                   act _ = return ()

locationsAndCounts :: String -> IO (Int, Int)
locationsAndCounts file = do
  hLog <- openFile file ReadMode
  serverEventList <- toListM $ parseProducer (fromHandle hLog)
  let locationEventsCount = length (filter isLocation serverEventList)
  let moveEventsCount = length (filter isMove serverEventList)
  return (locationEventsCount, moveEventsCount)
    where isLocation (Just (Right (LocationEvent _ _))) = True
          isLocation _ = False
          isMove (Just (Right (MoveEvent _))) = True
          isMove _ = False

expectedLocsAndMovesCounts :: String -> IO (Int, Int)
expectedLocsAndMovesCounts file = do
  expectedLocationsCount <- countStringsWith (isInfixOf "1;36m") file
  expectedMoveCount <- countStringsWith (isInfixOf $ encodeUtf8 "Вы поплелись") file
  return (expectedLocationsCount, expectedMoveCount)
  where countStringsWith predicate file = do contents <- readFile file
                                             return $ length $ filter predicate $ lines contents
