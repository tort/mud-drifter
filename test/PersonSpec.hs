{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module PersonSpec (spec) where

import Protolude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import PersonTask


spec :: Spec
spec = describe "Person" $ do
  it "can travel" pending
