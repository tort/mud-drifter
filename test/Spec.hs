{-# LANGUAGE OverloadedStrings #-}

import Mapper
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.ByteString
import Prelude hiding (readFile, length, filter, take)

main :: IO ()
main = do
    log <- readFile "log"
    hspec $ do
      describe "Mapper" $ do
        it "parse log to graph" $ countRoomsSimple log `shouldBe` countRoomsInGraph log
