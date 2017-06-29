{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.ByteString.Char8 hiding (filter, length)
import Test.Hspec.Attoparsec
import Pipes.ByteString hiding (filter, length)
import Prelude hiding (readFile, putStrLn)
import System.IO hiding (readFile, putStrLn)
import Pipes.Core
import Pipes.Prelude hiding (fromHandle, filter, length)

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
        it "parse multiple moves" $ do hLog <- openFile "test/logs/simpleWalk.log" ReadMode 
                                       serverEventList <- toListM $ parseProducer (fromHandle hLog)
                                       (length (filter isLocation serverEventList)) `shouldBe` (2 :: Int)
                                       (length (filter isMove serverEventList)) `shouldBe` 4
                                       where isLocation (Just (Right (Location _))) = True
                                             isLocation _ = False
                                             isMove (Just (Right (Move _ _))) = True
                                             isMove _ = False
