{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.ByteString.Char8
import Prelude hiding (readFile, length, filter, take)
import Test.Hspec.Attoparsec
import Parser

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
                                 log ~> serverInputParser `shouldParse` (Location 35040)
        it "patse move to location" $ pending
