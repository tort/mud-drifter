{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Person ( person
              , loadWorld
              , findDirection
              , travel
              ) where

import Protolude hiding (Location)
import Pipes
import qualified Data.Configurator as DC
import Data.Configurator.Types
import Mapper
import Event
import World
import Data.Maybe
import Control.Lens
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.List as L

person :: Monad m => World -> Pipe Event Event m ()
person world = travelTask world

travel :: MonadIO m => Set Direction -> [LocationId] -> Pipe Event Event m ()
travel directions path = go False path
    where go actionIssued [] = return ()
          go actionIssued [_] = return ()
          go actionIssued remainingPath@(from:to:xs) = await >>= \case
            PulseEvent -> (when (not actionIssued) (yield $ SendToServer $ fromJust $ direction from to)) >> go True remainingPath
            (ServerEvent (LocationEvent (Event.Location locationId _) _ _)) -> go False $ dropWhile (/= locationId) remainingPath
            _ -> go actionIssued remainingPath
          direction from to = trigger <$> findDirection directions from to

findMoveQuests :: Monad m => LocationId -> LocationId -> Pipe Event Event m ()
findMoveQuests from to = action $ L.lookup (from, to) travelActions
  where action (Just quest) = quest
        action _ = return ()

findDirection :: Set Direction -> LocationId -> LocationId -> Maybe Direction
findDirection directions from to = find (\d -> locIdFrom d == from && locIdTo d == to) directions

loadConfigProperty :: Text -> IO (Maybe Text)
loadConfigProperty propertyName = do conf <- DC.load [Required $ T.unpack personCfgFileName]
                                     propertyValue <- DC.lookup conf propertyName
                                     return propertyValue

personCfgFileName :: Text
personCfgFileName = "person.cfg"

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
