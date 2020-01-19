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

travelTo :: MonadIO m => Text -> World -> Pipe Event Event m ()
travelTo substr world = action findLocation
  where findLocation = findLocationsBy substr world
        action [] = liftIO $ putStrLn ("no matching locations found" :: Text)
        action [locTo] = (liftIO $ putStrLn ("travelling to " <> showt locTo)) >> travelAction locTo >>= printAfterFinish
        action _ = liftIO $ printLocations substr world
        travelAction to = findCurrentLoc >>= \from ->
          case findTravelPath from to (_worldMap world)
            of (Just path) -> travelPath path
               Nothing -> return (Failure "no path found")
        travelPath path = travel path world
        printAfterFinish result = liftIO $ putStrLn $ case result of Success -> "travel succeeded"
                                                                     Failure err -> err

findCurrentLoc :: MonadIO m => Pipe Event Event m LocationId
findCurrentLoc = yield (SendToServer "смотреть") >> go
  where go = await >>= \case evt@(ServerEvent (LocationEvent (Location currLoc _) _ _)) -> yield evt >> return currLoc
                             evt -> yield evt >> go

travel :: MonadIO m => [LocationId] -> World -> Pipe Event Event m (Result a)
travel path world = go path
    where go [] = return (Failure "path lost")
          go [_] = return Success
          go remainingPath@(from:to:xs) = (waitMove remainingPath >-> travelAction world from to) >>= \locationId -> go (dropWhile (/= locationId) remainingPath)
          waitMove remainingPath = await >>= \case (ServerEvent (LocationEvent (Location locationId _) _ _)) -> return locationId
                                                   evt -> yield evt >> waitMove remainingPath
