{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module DrifterSpec (spec) where

import Protolude
import Test.Hspec
import Test.QuickCheck
import Data.ByteString.Char8
import Data.Text
import Data.Text.Encoding
import Pipes
import Pipes.Prelude
import Event
import Drifter

spec :: Spec
spec = describe "MudDrifter" $ do
  it "print error in case of input without connection" pending
  it "send console input to mud server" pending
  it "/conn => connect to server" pending
  it "/zap => disconnect form server" pending
  it "logs raw server events" pending
  it "logs console input and server events" pending
  it "has prompt when not connected" pending
  it "does not connect if already connected" pending
  it "does not zap when disconnected" pending
