{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Person ( travel
              , travelTo
              , login
              , cover
              ) where

import Protolude hiding (Location)
import Pipes
import Data.Configurator.Types
import Mapper
import Event
import World
import Data.Maybe
import Control.Lens
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.List as L
import TextShow

login :: Pipe Event Event IO ()
login = await >>= \case (ServerEvent CodepagePrompt) -> yield (SendToServer "5") >> login
                        (ServerEvent LoginPrompt) -> yield (SendToServer "генод") >> login
                        (ServerEvent PasswordPrompt) -> yield (SendToServer "каркасный") >> login
                        (ServerEvent WelcomePrompt) -> yield (SendToServer "")
                        _ -> login

findCurrentLoc :: MonadIO m => Pipe Event Event m ServerEvent
findCurrentLoc = yield (SendToServer "смотреть") >> go
  where go = await >>= \case evt@(ServerEvent locEvt@LocationEvent{}) -> yield evt >> return locEvt
                             evt -> yield evt >> go

travel :: MonadIO m => [LocationId] -> ServerEvent -> World -> Pipe Event Event (ExceptT Text m) ServerEvent
travel path locationEvent world = go path locationEvent
  where go [] _ = lift $ throwError "path lost"
        go [_] locEvt = return locEvt
        go remainingPath@(from:to:xs) locEvtFrom = (waitMove remainingPath >-> travelAction world locEvtFrom to) >>= \locEvt@LocationEvent{} ->
                                                                              go (dropWhile (/= (_locationId $ _location locEvt)) remainingPath) locEvt
        waitMove remainingPath = await >>= \case (ServerEvent l@LocationEvent{}) -> return l
                                                 evt -> yield evt >> waitMove remainingPath

travelTo :: MonadIO m => Text -> World -> Pipe Event Event (ExceptT Text m) ServerEvent
travelTo substr world = action findLocation
  where findLocation = findLocationsBy substr world
        action [] = lift $ throwError "no matching locations found"
        action [locTo] = (liftIO $ putStrLn ("travelling to " <> showt locTo)) >> travelAction locTo
        action _ = (liftIO $ printLocations substr world) >> (lift $ throwError  "multiple locations found")
        travelAction to = findCurrentLoc >>= \currLocEvt@(LocationEvent (Event.Location from _) _ _ _) ->
          case findTravelPath from to (_worldMap world)
            of (Just path) -> travelPath path currLocEvt
               Nothing -> lift $ throwError "no path found"
        travelPath path currLocEvt = travel path currLocEvt world

cover :: MonadIO m => World -> Pipe Event Event m ServerEvent
cover world = awaitFightBegin >> return PromptEvent
  where awaitFightBegin = await >>= \evt -> yield evt >> case evt of (ServerEvent FightPromptEvent) -> awaitFightEnd
                                                                     _ -> awaitFightBegin
        awaitFightEnd = await >>= \case (ServerEvent PromptEvent) -> awaitFightBegin
                                        PulseEvent -> awaitFightEnd
                                        evt -> yield evt >> awaitFightEnd
