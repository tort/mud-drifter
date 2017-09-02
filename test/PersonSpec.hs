{-# LANGUAGE OverloadedStrings #-}

module PersonSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Person


spec :: Spec
spec = describe "Person" $ do
  it "provide locations by substring" pending
