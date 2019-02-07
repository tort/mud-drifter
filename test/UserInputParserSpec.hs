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
import RemoteConsole
import Event

spec :: Spec
spec = describe "UserInputParser" $ do
  it "parse non-command input" $ (parse userInputParser "" "blabla") `shouldBe` (Right $ ServerCommand "blabla")
  it "parse /findloc command" $ (parse userInputParser "" "/лок В таверне") `shouldBe` (Right $ FindLoc "В таверне")
  it "parse /findloc without param" $ (parse userInputParser "" "/лок") `shouldBe` (Right $ FindLoc "")
  it "parse /findloc without param" $ (parse userInputParser "" "/лок ") `shouldBe` (Right $ FindLoc "")
  it "parse /conn command" $ (parse userInputParser "" "/conn") `shouldBe` (Right $ Connect)
  it "parse /zap command" $ (parse userInputParser "" "/zap") `shouldBe` (Right $ Zap)
  it "parse /path regex" $ (parse userInputParser "" "/путь В избе") `shouldBe` (Right $ FindPathTo "В избе")
  it "parse /path toLocId" $ (parse userInputParser "" "/путь 34546") `shouldBe` (Right $ FindPathToLocId (LocationId 34546))
  it "parse /path fromLocId toLocId" $ (parse userInputParser "" "/путь 111 222") `shouldBe` (Right $ mkFindPathFromTo 111 222)
  it "parse /path      fromLocId       toLocId    " $ (parse userInputParser "" "/путь     1     2    ") `shouldBe` (Right $ mkFindPathFromTo 1 2)
  it "parse /path str toId" $ (parse userInputParser "" "/путь a 2") `shouldBe` (Right $ FindPathTo "a 2")
  it "parse /path fromId src" $ (parse userInputParser "" "/путь 1 b   ") `shouldBe` (Right $ FindPathTo "1 b")
  it "parse /path" $ isLeft $ (parse userInputParser "" "/путь")
  it "parse /go locId command" $ (parse userInputParser "" "/го 34546") `shouldBe` (Right $ GoToLocId $ LocationId 34546)
  it "parse /go regex command" $ (parse userInputParser "" "/го В избе") `shouldBe` (Right $ GoTo "В избе")
  it "parse empty user input" $ (parse userInputParser "" "") `shouldBe` (Right $ ServerCommand "")
  it "parse /where mob command" $ (parse userInputParser "" "/где моб муха") `shouldBe` (Right $ WhereMob "муха")
  it "return error in case of misspelled command" $ isLeft $ parse userInputParser "" "/unknowncommand blabla"

mkFindPathFromTo :: Int -> Int -> UserCommand
mkFindPathFromTo from to = FindPathFromTo (LocationId from) (LocationId to)
