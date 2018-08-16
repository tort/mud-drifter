{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module TravelTaskSpec (spec) where

import Protolude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Set hiding (foldl, filter)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable
import Control.Wire hiding ((.))
import qualified Control.Wire as W
import Control.Wire.Controller
import Control.Wire.Core
import Event
import qualified Event as E
import Control.Arrow
import Travel

spec :: Spec
spec = describe "travel task" $ do
  it "having no path, return pulses unused" pending
  it "pass all events as is, but pulses, which it may replace with command" $
    let inEvts = [TravelRequest path, locationEvent, PulseEvent, PulseEvent, PulseEvent, unknownEvent, PulseEvent]
        path = [1, 2, 3]
        locationEvent = ServerEvent $ LocationEvent (E.Location 2 "loc title") []
        unknownEvent = ServerInput "unknown event"
    in do outEvts <- pushToWire (toEvtList <<< travelTo <<< newEvent) (return . Just <$> inEvts)
          outEvts `shouldSatisfy` (\oe -> TravelRequest path `elem` oe && locationEvent `elem` oe)
  it "act by pulses only. number of actions == number of pulses" $
    let inEvts = [PulseEvent, TravelRequest [], PulseEvent, PulseEvent, ServerEvent LoginPrompt]
        inPulses = filter (== PulseEvent) inEvts
        outCommandsOrPulses = filter (\e -> e == PulseEvent || isCmd e)
        isCmd (ServerCommand _) = True
        isCmd _ = False
     in do outEvts <- pushToWire (toEvtList <<< travelTo <<< newEvent) (return . Just <$> inEvts)
           length (outCommandsOrPulses outEvts) `shouldBe` length inPulses
  it "issue move command after location event, where location belongs to remaining path" $
    let inEvts = [TravelRequest path, locationEvent, PulseEvent]
        path = [1, 2, 3]
        locationEvent = ServerEvent $ LocationEvent (E.Location 2 "loc title") []
    in do outEvts <- pushToWire (toEvtList <<< travelTo <<< newEvent) (return . Just <$> inEvts)
          outEvts `shouldBe` [TravelRequest path, locationEvent, ServerCommand "command"]
  it "does not repeat without delivery confirmation" $
    let inEvts = [TravelRequest path, locationEvent, PulseEvent, PulseEvent, PulseEvent, unknownEvent, PulseEvent]
        path = [1, 2, 3]
        locationEvent = ServerEvent $ LocationEvent (E.Location 2 "loc title") []
        unknownEvent = ServerInput "unknown event"
    in do outEvts <- pushToWire (toEvtList <<< travelTo <<< newEvent) (return . Just <$> inEvts)
          outEvts `shouldBe` [TravelRequest path, locationEvent, ServerCommand "command", PulseEvent, PulseEvent, unknownEvent, ServerCommand "command"]

pushToWire :: Monad m => Wire m a [b] -> [a] -> m [b]
pushToWire wire inEvts = fst <$> foldl runWire initAcc inEvts
  where initAcc = return ([], wire)
        runWire acc e = do (_, wr) <- acc
                           stepWire wr e

toEvtList :: Monad m => Wire m (W.Event a) [a]
toEvtList = proc inEvt -> do
  acc <- scan' [] -< (toAcc <$> inEvt)
  returnA -< acc
    where toAcc evt = \acc -> (acc ++ [evt])
