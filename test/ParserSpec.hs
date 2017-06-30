{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

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

import Parser
import Person

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
                                 log ~> serverInputParser `shouldParse` (Location (LocData 35040 "В корчме "))
        it "parse move to location" $ do log <- readFile "test/logs/move.log"
                                         log ~> serverInputParser `shouldParse` (Move "юг" (LocData 35039 "Во дворе перед корчмой "))
        it "parse move in darkness" $ do log <- readFile "test/logs/inDarkness.log"
                                         log ~> serverInputParser `shouldParse` (Move "север" (LocData 5200 "Лесная дорога "))
        it "parse multiple moves" $ do hLog <- openFile simpleWalkFile ReadMode 
                                       serverEventList <- toListM $ parseProducer (fromHandle hLog)
                                       expectedLocationsCount <- countStringsWith "1;36m" simpleWalkFile
                                       expectedMoveCount <- countStringsWith (encodeUtf8 "Вы поплелись") simpleWalkFile
                                       let locationEventsCount = length (filter isLocation serverEventList)
                                       let moveEventsCount = length (filter isMove serverEventList)
                                       moveEventsCount `shouldBe` expectedMoveCount
                                       (locationEventsCount + moveEventsCount) `shouldBe` expectedLocationsCount
                                       where isLocation (Just (Right (Location _))) = True
                                             isLocation _ = False
                                             isMove (Just (Right (Move _ _))) = True
                                             isMove _ = False
                                             countStringsWith substr file = do contents <- readFile file
                                                                               return $ length $ filter (isInfixOf substr) $ lines contents
                                             countStringsStartingWith substr file =  do contents <- readFile file
                                                                                        return $ length $ filter (isPrefixOf substr) $ lines contents
                                             simpleWalkFile = "test/logs/simpleWalk.log"
