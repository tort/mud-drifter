{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.ByteString.Char8
import Prelude hiding (readFile, length, filter, take)
import Test.Hspec.Attoparsec
import Parser

main :: IO ()
main = do
    log <- readFile "test/common.log"
    codepagePromptLog <- readFile "test/codepagePrompt.log"
    loginPromptLog <- readFile "test/loginPrompt.log"
    passwordPromptLog <- readFile "test/passwordPrompt.log"
    welcomePromptLog <- readFile "test/welcomePrompt.log"
    hspec $ do
      describe "Parser" $ do
        it "does not fail on input" $ shouldSucceedOn serverInputParser log
        it "parse codepage prompt" $ codepagePromptLog ~> serverInputParser `shouldParse` CodepagePrompt
        it "login prompt" $ loginPromptLog ~> serverInputParser `shouldParse` LoginPrompt
        it "password prompt" $ passwordPromptLog ~> serverInputParser `shouldParse` PasswordPrompt
        it "welcome prompt" $ welcomePromptLog ~> serverInputParser `shouldParse` WelcomePrompt
        --it "parse location and move to location" $ parsedEvents `shouldBe` expected
