{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Person ( person
              , loadWorld
              , findDirection
              , travel
              , travelTo
              , findCurrentLoc
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

person :: Monad m => World -> Pipe Event Event m ()
person world = undefined

travelTo :: MonadIO m => Text -> World -> Pipe Event Event m ()
travelTo substr world = action findLocation >> printAfterFinish
  where findLocation = locationBy substr world
        action [] = liftIO $ putStrLn ("no matching locations found" :: Text)
        action [locTo] = (liftIO $ putStrLn ("travelling to " <> showt locTo)) >> travelAction locTo
        action _ = liftIO $ printLocations substr world
        travelAction to = findCurrentLoc >>= \from ->
          case findTravelPath from to (_worldMap world)
            of (Just path) -> travelPath path
               Nothing -> liftIO $ putStrLn ("no path found" :: Text)
        travelPath path = travel path world
        printAfterFinish = liftIO $ purStrLn ("travel succeeded" :: Text)

findCurrentLoc :: MonadIO m => Pipe Event Event m LocationId
findCurrentLoc = yield (SendToServer "смотр") >> go
  where go = await >>= \case evt@(ServerEvent (LocationEvent (Location currLoc _) _ _)) -> yield evt >> return currLoc
                             evt -> yield evt >> go

travel :: MonadIO m => [LocationId] -> World -> Pipe Event Event m ()
travel path world = go False path
    where go actionIssued [] = return ()
          go actionIssued [_] = return ()
          go actionIssued remainingPath@(from:to:xs) = await >>= \case
            PulseEvent -> (when (not actionIssued) (yield $ SendToServer $ fromJust $ direction from to)) >> go True remainingPath
            (ServerEvent (LocationEvent (Location locationId _) _ _)) -> go False $ dropWhile (/= locationId) remainingPath
            _ -> go actionIssued remainingPath
          direction from to = trigger <$> findDirection (_directions world) from to

findMoveQuests :: Monad m => LocationId -> LocationId -> Pipe Event Event m ()
findMoveQuests from to = action $ L.lookup (from, to) travelActions
  where action (Just quest) = quest
        action _ = return ()

findDirection :: Set Direction -> LocationId -> LocationId -> Maybe Direction
findDirection directions from to = find (\d -> locIdFrom d == from && locIdTo d == to) directions

travelActions :: Monad m => [((LocationId, LocationId), (Pipe Event Event m ()))]
travelActions = [ ((LocationId 5102, LocationId 5103), openDoor South)
                , ((LocationId 5103, LocationId 5102), openDoor North)
                , ((LocationId 5104, LocationId 5117), setupLadder)
                , ((LocationId 5052, LocationId 4064), payOldGipsy)
                , ((LocationId 4064, LocationId 5052), payYoungGipsy)
                ]

payOldGipsy :: Monad m => Pipe Event Event m ()
payOldGipsy = undefined

payYoungGipsy :: Monad m => Pipe Event Event m ()
payYoungGipsy = undefined

setupLadder :: Monad m => Pipe Event Event m ()
setupLadder = (yield $ SendToServer "смотреть") >> waitLocEvt
  where waitLocEvt = await >>= checkItemRoomDescs
        checkItemRoomDescs (ServerEvent (LocationEvent loc objs mobs)) = if elem (ItemRoomDesc "На полу лежит лестница.") objs
                                                                    then yield $ SendToServer "приставить лестница"
                                                                    else return ()
        checkItemRoomDescs _ = waitLocEvt

openDoor :: Monad m => RoomDir -> Pipe Event Event m ()
openDoor dir = (yield $ SendToServer ("смотреть " <> show dir)) >> waitObstacleEvent
  where waitObstacleEvent = await >>= checkObstacleEvent
        checkObstacleEvent (ServerEvent (ObstacleEvent _ obstacle)) = yield $ SendToServer ("открыть " <> obstacle <> " " <> show dir)
        checkObstacleEvent (ServerEvent (UnknownServerEvent "")) = return ()
        checkObstacleEvent (ServerEvent (GlanceEvent _ _ _)) = return ()
        checkObstacleEvent _ = waitObstacleEvent
