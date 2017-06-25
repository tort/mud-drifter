{-# LANGUAGE OverloadedStrings #-}

import Mapper
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.ByteString
import Prelude hiding (readFile, length, filter, take)

main :: IO ()
main = do
    hspec $ do
      describe "Parser" $ do
        it "parse codepage prompt, login prompt, password prompt, welcome prompt" $ parsedEvents `shouldBe` expectedLoginEvents
        it "parse location and move to location" $ parsedEvents `shouldBe` expected
