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
import Text.Parsec.Error
import Data.Either

import UserInputParser
import Person
import RemoteConsole

spec :: Spec
spec = describe "UserInputParser" $ do
  it "parse non-command input" $ (parse userInputParser "" "blabla") `shouldBe` (Right $ UserInput "blabla")
  it "parse /findloc command" $ (parse userInputParser "" "/findloc В таверне") `shouldBe` (Right $ FindLoc "В таверне")
  it "parse /findloc without param" $ (parse userInputParser "" "/findloc") `shouldBe` (Right $ FindLoc "")
  it "parse /findloc without param" $ (parse userInputParser "" "/findloc ") `shouldBe` (Right $ FindLoc "")
  it "parse /conn command" $ (parse userInputParser "" "/conn") `shouldBe` (Right $ KeepConn True)
  it "parse /unconn command" $ (parse userInputParser "" "/unconn") `shouldBe` (Right $ KeepConn False)
  it "parse /path regex" $ (parse userInputParser "" "/path В избе") `shouldBe` (Right $ FindPathTo "В избе")
  it "parse /path toLocId" $ (parse userInputParser "" "/path 34546") `shouldBe` (Right $ FindPathToLocId 34546)
  it "parse /path fromLocId toLocId" $ (parse userInputParser "" "/path 111 222") `shouldBe` (Right $ FindPathFromTo 111 222)
  it "parse /path      fromLocId       toLocId    " $ (parse userInputParser "" "/path     1     2    ") `shouldBe` (Right $ FindPathFromTo 1 2)
  it "parse /path str toId" $ (parse userInputParser "" "/path a 2") `shouldBe` (Right $ FindPathTo "a 2")
  it "parse /path fromId src" $ (parse userInputParser "" "/path 1 b   ") `shouldBe` (Right $ FindPathTo "1 b")
  it "parse /path" $ isLeft $ (parse userInputParser "" "/path")
  it "parse /go locId command" $ (parse userInputParser "" "/go 34546") `shouldBe` (Right $ GoToLocId 34546)
  it "parse /go regex command" $ (parse userInputParser "" "/go В избе") `shouldBe` (Right $ GoTo "В избе")
  it "parse empty user input" $ (parse userInputParser "" "") `shouldBe` (Right $ UserInput "")
  it "return error in case of misspelled command" $ isLeft $ parse userInputParser "" "/unknowncommand blabla"
