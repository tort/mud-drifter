{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Person ( travel
              , travelTo
              , login
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

data Travel

login :: Pipe Event Event IO ()
login = await >>= \case (ServerEvent CodepagePrompt) -> yield (SendToServer "5") >> login
                        (ServerEvent LoginPrompt) -> yield (SendToServer "генод") >> login
                        (ServerEvent PasswordPrompt) -> yield (SendToServer "каркасный") >> login
                        (ServerEvent WelcomePrompt) -> yield (SendToServer "")
                        _ -> login

travelTo :: MonadIO m => Text -> World -> Pipe Event Event m (Result ServerEvent)
travelTo substr world = action findLocation
  where findLocation = findLocationsBy substr world
        action [] = return $ Failure "no matching locations found"
        action [locTo] = (liftIO $ putStrLn ("travelling to " <> showt locTo)) >> travelAction locTo
        action _ = (liftIO $ printLocations substr world) >> return (Failure "multiple locations found")
        travelAction to = findCurrentLoc >>= \currLocEvt@(LocationEvent (Location from _) _ _) ->
          case findTravelPath from to (_worldMap world)
            of (Just path) -> travelPath path currLocEvt
               Nothing -> return (Failure "no path found")
        travelPath path currLocEvt = travel path currLocEvt world

findCurrentLoc :: MonadIO m => Pipe Event Event m ServerEvent
findCurrentLoc = yield (SendToServer "смотреть") >> go
  where go = await >>= \case evt@(ServerEvent locEvt@LocationEvent{}) -> yield evt >> return locEvt
                             evt -> yield evt >> go

travel :: MonadIO m => [LocationId] -> ServerEvent -> World -> Pipe Event Event m (Result ServerEvent)
travel path locationEvent world = go path locationEvent
  where go [] _ = return (Failure "path lost")
        go [_] locEvt = return $ Success locEvt
        go remainingPath@(from:to:xs) locEvtFrom = (waitMove remainingPath >-> travelAction world locEvtFrom to) >>= \locEvt@LocationEvent{} ->
                                                     go (dropWhile (/= (_locationId $ _location locEvt)) remainingPath) locEvt
        waitMove remainingPath = await >>= \case (ServerEvent l@LocationEvent{}) -> return l
                                                 evt -> yield evt >> waitMove remainingPath
