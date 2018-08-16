{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Travel ( travelTo
              ) where

import Protolude
import Event
import Control.Wire hiding ((.))
import qualified Control.Wire as W
import qualified Event as E
import Control.Arrow

travelTo :: Monad m => Wire m (W.Event E.Event) (W.Event E.Event)
travelTo = proc inEvt -> do
  bPath <- hold' [] -< unwrapPath <$> filterE isTravelRequest inEvt
  bLoc <- hold' offlineLocationId -< unwrapLocId <$> filterE isLocation inEvt
  arr (snd <$>) <<< scanE (False, PulseEvent) -< throttle . issueCmd bPath <$> inEvt
    where isTravelRequest (TravelRequest _) = True
          isTravelRequest _ = False
          isLocation (ServerEvent (LocationEvent _ _)) = True
          isLocation _ = False
          unwrapPath (TravelRequest path) = path
          unwrapLocId (ServerEvent (LocationEvent loc _)) = locId loc
          offlineLocationId = 0 :: LocId
          issueCmd path PulseEvent = case path of x:y:xs -> ServerCommand "command"
                                                  _ -> PulseEvent
          issueCmd _ e = e
          throttle e@(ServerCommand _) = \acc -> case acc of (True, _) -> (True, PulseEvent)
                                                             (False, _) -> (True, e)
          throttle e@(ServerInput _) = const (False, e)
          throttle e = \(flag, _) -> (flag, e)
