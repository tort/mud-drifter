{-# LANGUAGE OverloadedStrings #-}

module UserInputParserSpec (spec) where

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
import Text.Parsec
import Text.Parsec.Text
import Data.Either

import UserInputParser
import Person
import RemoteConsole

spec :: Spec
spec = describe "UserInputParser" $ do
  it "return UserInput if input is not a command" $ (parse userInputParser "" "blabla") `shouldBe` (Right $ UserInput "blabla")
