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
                                 log ~> serverInputParser `shouldParse` (LocationEvent (Location 35040 "В корчме "))
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
                                                     serverEventList `shouldBe` [MoveEvent "восток", (LocationEvent $ Location 5112 "На кухне")]
                                                     hClose hLog
        it "parse equipment list" $ do log <- readFile "test/logs/listEquipment.log"
                                       log ~> serverInputParser `shouldParse` (ListEquipment [ EquipmentItem (Body, "легкий латный доспех", Excellent)
                                                                                             , EquipmentItem (Head, "легкий латный шлем", Excellent)
                                                                                             , EquipmentItem (Legs, "легкие латные поножи", Excellent)
                                                                                             , EquipmentItem (Waist, "холщовый мешок", Excellent)
                                                                                             , EquipmentItem (RightHand, "длинный бронзовый меч", Excellent)
                                                                                             ])

moveOrLocation :: ServerEvent -> Bool
moveOrLocation (MoveEvent _) = True
moveOrLocation (LocationEvent _) = True
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
  where isLocation (Just (Right (LocationEvent _))) = True
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
