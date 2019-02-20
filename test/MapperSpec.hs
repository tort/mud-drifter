{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MapperSpec (spec) where

import Protolude hiding (Location)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances

spec :: Spec
spec = describe "Mapper" $ do
  it "can find path from any location to any location" pending
  it "map world data to graph" pending
  it "fold move events to directions" $ pending
