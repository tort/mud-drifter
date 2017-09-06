{-# LANGUAGE OverloadedStrings #-}

module ServerInputParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.ByteString.Char8 hiding (filter, length)
import Test.Hspec.Attoparsec
import Pipes.ByteString hiding (filter, length, lines)
import Prelude hiding (readFile, putStrLn, lines)
import System.IO hiding (readFile, putStrLn, hGetContents)
import Pipes.Core
import Pipes.Prelude hiding (fromHandle, filter, length)
import Data.Text hiding (isInfixOf, isPrefixOf, length, filter, lines)
import Data.Text.Encoding

import ServerInputParser
import Person
import RemoteConsole

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
                                         log ~> serverInputParser `shouldParse` (MoveEvent "юг" (Location 35039 "Во дворе перед корчмой "))
        it "parse move in darkness" $ do log <- readFile "test/logs/inDarkness.log"
                                         log ~> serverInputParser `shouldParse` (MoveEvent "север" (Location 5200 "Лесная дорога "))
        it "parse log, starting from partial move message" $ do let simpleWalkFile = "test/logs/startingInTheMiddleOfMove.log"
                                                                (locationEventsCount, moveEventsCount) <- locationsAndCounts simpleWalkFile
                                                                (expectedLocationsCount, expectedMoveCount) <- expectedLocsAndMovesCounts simpleWalkFile
                                                                moveEventsCount `shouldBe` expectedMoveCount
                                                                (locationEventsCount + moveEventsCount) `shouldBe` expectedLocationsCount
        it "parse log, finishing on partial move message" $ do let simpleWalkFile = "test/logs/finishingInTheMiddleOfMove.log"
                                                               (locationEventsCount, moveEventsCount) <- locationsAndCounts simpleWalkFile
                                                               (expectedLocationsCount, expectedMoveCount) <- expectedLocsAndMovesCounts simpleWalkFile
                                                               moveEventsCount `shouldBe` (expectedMoveCount - 1)
                                                               (locationEventsCount + moveEventsCount) `shouldBe` (expectedLocationsCount - 1)
        it "parse multiple moves" $ do let simpleWalkFile = "test/logs/simpleWalk.log"
                                       (locationEventsCount, moveEventsCount) <- locationsAndCounts simpleWalkFile
                                       (expectedLocationsCount, expectedMoveCount) <- expectedLocsAndMovesCounts simpleWalkFile
                                       moveEventsCount `shouldBe` expectedMoveCount
                                       (locationEventsCount + moveEventsCount) `shouldBe` expectedLocationsCount
        {-it "parse remote console input" $ do hLog <- openFile "test/logs/inDarkness.log" ReadMode
                                             events <- toListM $ parseRemoteInput2 (fromHandle hLog)
                                             events `shouldBe` ["", "blabla"]-}

locationsAndCounts :: String -> IO (Int, Int)
locationsAndCounts file = do
  hLog <- openFile file ReadMode
  serverEventList <- toListM $ parseProducer (fromHandle hLog)
  let locationEventsCount = length (filter isLocation serverEventList)
  let moveEventsCount = length (filter isMove serverEventList)
  return (locationEventsCount, moveEventsCount)
  where isLocation (Just (Right (LocationEvent _))) = True
        isLocation _ = False
        isMove (Just (Right (MoveEvent _ _))) = True
        isMove _ = False

expectedLocsAndMovesCounts :: String -> IO (Int, Int)
expectedLocsAndMovesCounts file = do
  expectedLocationsCount <- countStringsWith (isInfixOf "1;36m") file
  expectedMoveCount <- countStringsWith (isInfixOf $ encodeUtf8 "Вы поплелись") file
  return (expectedLocationsCount, expectedMoveCount)
  where countStringsWith predicate file = do contents <- readFile file
                                             return $ length $ filter predicate $ lines contents
