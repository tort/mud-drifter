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
        it "does not fail on input" $ do log <- readFile "test/common.log"
                                         shouldSucceedOn serverInputParser log
        it "parse codepage prompt" $ do log <- readFile "test/codepagePrompt.log"
                                        log ~> serverInputParser `shouldParse` CodepagePrompt
        it "login prompt" $ do log <- readFile "test/loginPrompt.log"
                               log ~> serverInputParser `shouldParse` LoginPrompt
        it "password prompt" $ do log <- readFile "test/passwordPrompt.log"
                                  log ~> serverInputParser `shouldParse` PasswordPrompt
        it "welcome prompt" $ do log <- readFile "test/welcomePrompt.log"
                                 log ~> serverInputParser `shouldParse` WelcomePrompt
        it "parse location and move to location" $ pending
