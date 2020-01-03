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
import Pipes.Safe
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

travelTask :: Monad m => World -> Pipe Event Event m ()
travelTask world = waitTravelRequest Nothing
  where afterTravel locId (Failure err loc) = yield (ConsoleOutput $ "travel to " <> (encodeUtf8 $ showVal locId) <> " failed: " <> err) >> waitTravelRequest loc
        afterTravel locId (Success loc) = yield (ConsoleOutput $ "travel to " <> (encodeUtf8 $ showVal locId) <> " finished") >> waitTravelRequest (Just loc)
        waitTravelRequest currLoc = await >>= \evt -> case evt of (UserCommand (GoToLocId locId)) -> travelPath currLoc locId >>= afterTravel locId
                                                                  (ServerEvent (LocationEvent (Location locId _) _ _)) -> yield evt >> waitTravelRequest (Just locId)
                                                                  (ServerEvent DarknessEvent) -> yield evt >> waitTravelRequest Nothing
                                                                  _ -> yield evt >> waitTravelRequest currLoc
        travelPath (Just currLoc) locId = travel world $ findTravelPath currLoc locId (_worldMap world)
        travelPath Nothing _ = (return $ Failure "current location is unknown" Nothing)

type Reason = ByteString
data TravelResult = Success LocationId | Failure Reason (Maybe LocationId) deriving (Eq, Show)

travel :: Monad m => World -> Maybe [LocationId] -> Pipe Event Event m TravelResult
travel world Nothing = return $ Failure "there is no path there" Nothing
travel world (Just [locId]) = return $ Success locId
travel world (Just path) = makeStep
  where chopPath locId path = dropWhile (/=locId) path :: [LocationId]
        go (from:to:xs) = do findMoveQuests from to
                             mapM_ (yield . SendToServer) (trigger <$> (findDirection (_directions world) from to))
        go [_] = return ()
        go [] = return ()
        makeStep = await >>= \evt -> case evt of PulseEvent -> go path >> waitMove
                                                 e -> yield e >> makeStep
        waitMove = await >>= handleLocationEvent
        handleLocationEvent evt@(ServerEvent (LocationEvent loc _ _)) = yield evt >> if elem (loc ^. locationId) path
                                                                                        then travel world (Just $ chopPath (loc ^. locationId) path)
                                                                                        else return $ Failure "path lost" (Just (loc ^. locationId))
        handleLocationEvent evt@(ServerEvent (MoveEvent dir)) = let from:to:xs = path
                                                                 in yield evt >> case ((== dir) <$> trigger <$> (findDirection (_directions world) from to))
                                                                                      of Nothing -> waitMove
                                                                                         (Just False) -> return $ Failure "path lost" Nothing
                                                                                         (Just True) -> waitMove
        handleLocationEvent evt@(ServerEvent CantGoDir) = yield evt >> (return $ Failure "path lost" Nothing)
        handleLocationEvent evt@(ServerEvent DarknessEvent) = yield evt >> travel world (Just (L.tail path))
        handleLocationEvent PulseEvent = waitMove
        handleLocationEvent e = yield e >> waitMove

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
