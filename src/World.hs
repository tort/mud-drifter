{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module World ( locsByRegex
             , showLocs
             , loadWorld
             , parseServerEvents
             , travelAction
             , World(..)
             , Direction(..)
             , Trigger(..)
             , WorldMap
             ) where

import Protolude hiding (Location, runStateT, Down)
import qualified Data.ByteString.Char8 as C8
import ServerInputParser
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import System.IO (hClose)
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as PP
import Pipes.Attoparsec
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PBS
import qualified Data.Text as T
import Data.Text.Encoding()
import Data.Either
import Data.Maybe
import Debug.Trace
import Event
import Data.Map.Strict()
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import Data.Set()
import qualified Data.Set as S
import Logger
import System.Directory
import Control.Lens
import TextShow
import TextShow.Generic
import Text.Regex.TDFA

data World = World { _worldMap :: WorldMap
                   , _locations :: Set Location
                   , _directions :: Set Direction
                   , _itemsOnMap :: Map ServerEvent (Map LocationId Int)
                   , _itemStats :: [ItemStats]
                   , _mobsDiscovered :: Map MobRoomDesc (Map LocationId Int)
                   , _mobStats :: [Mob]
                   , _questActions :: Map (LocationId, LocationId) [Event]
                   }

data Direction = Direction { locIdFrom :: LocIdFrom
                           , locIdTo :: LocIdTo
                           , trigger :: Trigger
                           }
                           deriving (Eq, Ord, Generic)
                           deriving TextShow via FromGeneric Direction

type LocIdFrom = LocationId
type LocIdTo = LocationId
type Trigger = Text
type WorldMap = Gr () Int

unwrapRight :: Either ParsingError ServerEvent -> ServerEvent
unwrapRight (Right val) = val

accDirections :: Set Direction -> [ServerEvent] -> Set Direction
accDirections directions pair =
  let updateWorld locFrom locTo dir
        | locFrom == locTo = directions
        | otherwise = insertOpposite $ insertAhead directions
        where insertAhead = S.insert (Direction (locFrom^.locationId) (locTo^.locationId) dir)
              insertOpposite = S.insert (Direction (locTo^.locationId) (locFrom^.locationId) $ oppositeDir dir)
   in case pair of [LocationEvent locTo _ _, MoveEvent dir, LocationEvent locFrom _ _] -> updateWorld locFrom locTo dir
                   _ -> directions

oppositeDir :: Text -> Text
oppositeDir "вверх" = "вниз"
oppositeDir "вниз" = "вверх"
oppositeDir "север" = "юг"
oppositeDir "юг" = "север"
oppositeDir "запад" = "восток"
oppositeDir "восток" = "запад"

mappableMove :: [ServerEvent] -> Bool
mappableMove [LocationEvent{}, MoveEvent _, LocationEvent{}] = True
mappableMove _ = False

showLocs :: [Location] -> ByteString
showLocs locs = encodeUtf8 $ renderMsg locs
  where renderMsg = addRet . joinToOneMsg . renderLocs
        joinToOneMsg = T.intercalate "\n"
        renderLocs = fmap renderLoc
        renderLoc node = (showVal (node^.locationId) <> " " <> showVal (node^.locationTitle)) :: Text
        addRet txt = T.snoc txt '\n'

locsByRegex :: World -> Text -> [Location]
locsByRegex world regex = S.toList $ S.filter (T.isInfixOf regex . T.toLower . (\l -> showVal $ l^.locationTitle)) locs
  where locs = _locations world

loadServerEvents :: FilePath -> Producer ByteString IO ()
loadServerEvents file = openfile >>= \h -> PBS.fromHandle h >> closefile h
  where openfile = lift $ openFile file ReadMode
        closefile h = lift $ hClose h

binEvtLogParser :: Producer ByteString IO () -> Producer Event IO ()
binEvtLogParser bsp = parseEventLogProducer =<< lift (PBS.toLazyM bsp) >-> PP.filter filterTravelActions

loadLogs :: [FilePath] -> Producer ByteString IO ()
loadLogs files = F.foldl' (\evtPipe file -> evtPipe >> loadServerEvents file) (return ()) files

extractItems :: Monad m => Pipe ServerEvent [ItemRoomDesc] m ()
extractItems = PP.filter isLocationEvent >-> PP.map (\(LocationEvent _ objects _) -> objects)

extractMobs :: Monad m => Pipe ServerEvent [MobRoomDesc] m ()
extractMobs = PP.filter isLocationEvent >-> PP.map (\(LocationEvent _ _ mobs) -> mobs)

extractDiscovered :: (Monad m, Ord a) => (ServerEvent -> [a]) -> Producer ServerEvent m () -> m (Map a (Map LocationId Int))
extractDiscovered entityExtractor producer = PP.fold toMap M.empty identity (producer >-> PP.filter isLocationEvent)
  where toMap acc evt@(LocationEvent (Location locId _) _ mobs) = F.foldl (insertMob locId) acc (entityExtractor evt)
        insertMob locId acc mob = M.alter (updateCount locId) mob acc
        updateCount locId Nothing = Just (M.insert locId 1 M.empty)
        updateCount locId (Just locToCountMap) = Just (M.alter plusOne locId locToCountMap)
        plusOne Nothing = Just 1
        plusOne (Just x) = Just (x + 1)

discoverItems :: (Monad m) => Producer ServerEvent m () -> m (Map ServerEvent (Map LocationId Int))
discoverItems producer = foldToMap (producer >-> filterMapDiscoveries >-> scanEvtWithLoc Nothing)
  where filterMapDiscoveries = forever $ await >>= \case
              evt@(LocationEvent (Location locId _) items _) -> yield evt >> mapM_ (yield . ItemInTheRoom) items
              evt@LootCorpse{} -> yield evt
              evt@TakeFromContainer{} -> yield evt
              evt@MobGaveYouItem{} -> yield evt
              _ ->  return ()
        scanEvtWithLoc maybeLocId = await >>= \case (LocationEvent (Location locId _) _ _) -> scanEvtWithLoc (Just locId)
                                                    evt -> case maybeLocId of Nothing -> scanEvtWithLoc Nothing
                                                                              (Just locId) -> yield (evt, locId) >> scanEvtWithLoc maybeLocId
        foldToMap = PP.fold toMap M.empty identity
        toMap acc (evt, locId) = insertMob locId acc evt
        insertMob locId acc mob = M.alter (updateCount locId) mob acc
        updateCount locId Nothing = Just (M.insert locId 1 M.empty)
        updateCount locId (Just locToCountMap) = Just (M.alter plusOne locId locToCountMap)
        plusOne Nothing = Just 1
        plusOne (Just x) = Just (x + 1)


extractLocs :: Monad m => Producer ServerEvent m () -> m (Set Location)
extractLocs serverEvtProducer = PP.fold toSet S.empty identity $ serverEvtProducer >-> PP.filter isLocationEvent >-> PP.map (\(LocationEvent loc _ _) -> loc)
  where toSet acc item = S.insert item acc

extractItemStats :: Monad m => Producer ServerEvent m () -> Producer ItemStats m ()
extractItemStats serverEvtProducer = serverEvtProducer >-> PP.filter isItemStatsEvent >-> PP.map (\(ItemStatsEvent item) -> item)

extractDirections :: Monad m => Producer ServerEvent m () -> m (Set Direction)
extractDirections producer = PP.fold accDirections S.empty identity (producer >-> PP.filter (\evt -> isLocationEvent evt || isMoveEvent evt)
                                                                                    >-> PP.scan toTriples [] identity
                                                                                    >-> PP.filter mappableMove)
                                                                                      where toTriples acc event
                                                                                              | length acc < 3 = event : acc
                                                                                              | otherwise = event : take 2 acc

listFilesIn :: FilePath -> IO [FilePath]
listFilesIn dir = ((dir ++ ) <$>) <$> listDirectory dir

loadWorld :: FilePath -> IO World
loadWorld currentDir = do
  serverLogFiles <- listFilesIn (currentDir ++ "/" ++ serverLogDir)
  evtLogFiles <- listFilesIn (currentDir ++ "/" ++ evtLogDir)
  directions <- (extractDirections . parseServerEvents . loadLogs) serverLogFiles
  locations <- (extractLocs . parseServerEvents . loadLogs) serverLogFiles
  itemsStats <- PP.toListM $ (extractItemStats . parseServerEvents . loadLogs) serverLogFiles
  itemsOnMap <- (discoverItems . parseServerEvents . loadLogs) serverLogFiles
  mobsOnMap <- ((extractDiscovered _mobs) . parseServerEvents . loadLogs) serverLogFiles
  questActions <- (obstacleActions . binEvtLogParser . loadLogs) evtLogFiles
  let worldMap = buildMap directions
   in return World { _worldMap = worldMap
                     , _locations = locations
                     , _directions = directions
                     , _itemsOnMap = itemsOnMap
                     , _itemStats = itemsStats
                     , _mobsDiscovered = mobsOnMap
                     , _mobStats = []
                     , _questActions = questActions
                     }

printWorldStats :: World -> Producer Event IO ()
printWorldStats world = yield $ ConsoleOutput worldStats
  where worldStats = encodeUtf8 $ locationsStats <> directionsStats <> items <> itemsStats <> mobs
        locationsStats = (show . length . _locations) world <> " локаций найдено\n"
        directionsStats = (show . length . _directions) world <> " переходов между локациями\n"
        items = (show . length . _itemsOnMap) world <> " предметов найдено\n"
        itemsStats = (show . length . _itemStats) world <> " предметов опознано\n"
        mobs = (show . length . _mobsDiscovered) world <> " мобов найдено\n"

parseServerEvents :: Producer ByteString IO () -> Producer ServerEvent IO ()
parseServerEvents src = PA.parsed serverInputParser src >>= onEndOrError
  where onEndOrError Right{} = liftIO $ print "Server stream finished"
        onEndOrError (Left (err, producer)) = (liftIO $ print "error when parsing") >> (yield $ ParseError $ errDesc err)
        errDesc (ParsingError ctxts msg) = "error: " <> C8.pack msg <> C8.pack (concat ctxts) <> "\n"

buildMap :: Set Direction -> Gr () Int
buildMap directions = mkGraph nodes edges
  where edges = concat $ (\d -> [aheadEdge d, reverseEdge d]) <$> vertexPairs
        nodes = (\n -> (n, ())) <$> F.foldl (\acc (Direction (LocationId fromId) (LocationId toId) _) -> fromId : toId : acc) [] directions
        directionVertexes (Direction (LocationId fromId) (LocationId toId) _) = (fromId, toId)
        questActionVertexes ((LocationId fromId), (LocationId toId)) = (fromId, toId)
        aheadEdge (fromId, toId) = (fromId, toId, 1)
        reverseEdge (fromId, toId) = (toId, fromId, 1)
        vertexPairs = directionsPairs ++ questActionsPairs
        directionsPairs = directionVertexes <$> (S.toList directions)
        questActionsPairs = questActionVertexes <$> M.keys (travelActions :: Map (LocationId, LocationId) (Pipe Event Event IO LocationId))

travelActions :: Monad m => Map (LocationId, LocationId) (Pipe Event Event m LocationId)
travelActions = M.fromList [ ((LocationId 5102, LocationId 5103), openDoor South)
                           , ((LocationId 5103, LocationId 5102), openDoor North)
                           , ((LocationId 5104, LocationId 5117), setupLadder)
                           , ((LocationId 5052, LocationId 4064), payOldGipsy)
                           , ((LocationId 4064, LocationId 5052), payYoungGipsy)
                           ]

directionActions :: Monad m => World -> Map (LocationId, LocationId) (Pipe Event Event m LocationId)
directionActions world = M.fromList $ directionVertexes <$> (S.toList $ _directions world)
  where directionVertexes (Direction from to dir) = ((from, to), movePipe dir)
        movePipe dir = await >>= \case PulseEvent -> yield (SendToServer dir) >> waitLocation
                                       _ -> movePipe dir
        waitLocation = await >>= \case (ServerEvent (LocationEvent (Location locationId _) _ _)) -> return locationId
                                       _ -> waitLocation

allActions :: Monad m => World -> Map (LocationId, LocationId) (Pipe Event Event m LocationId)
allActions world = M.union (directionActions world) travelActions

travelAction :: Monad m => World -> LocationId -> LocationId -> Pipe Event Event m LocationId
travelAction world from to = case M.lookup (from, to) (allActions world) of Nothing -> return to
                                                                            (Just pipe) -> pipe

payOldGipsy :: Monad m => Pipe Event Event m LocationId
payOldGipsy = move
  where move = await >>= \case PulseEvent -> (yield $ SendToServer $ "дать 1 кун цыган") >> waitLocation
                               _ -> move
        waitLocation = await >>= \case (ServerEvent (LocationEvent (Location locationId _) _ _)) -> return locationId
                                       _ -> waitLocation

payYoungGipsy :: Monad m => Pipe Event Event m LocationId
payYoungGipsy = move
  where move = await >>= \case PulseEvent -> (yield $ SendToServer $ "дать 1 кун цыган") >> waitLocation
                               _ -> move
        waitLocation = await >>= \case (ServerEvent (LocationEvent (Location locationId _) _ _)) -> return locationId
                                       _ -> waitLocation

setupLadder :: Monad m => Pipe Event Event m LocationId
setupLadder = (yield $ SendToServer "смотреть") >> waitLocEvt >> return (LocationId 1)
  where waitLocEvt = await >>= checkItemRoomDescs
        checkItemRoomDescs (ServerEvent (LocationEvent loc objs mobs)) = if elem (ItemRoomDesc "На полу лежит лестница.") objs
                                                                            then yield $ SendToServer "приставить лестница"
                                                                            else return ()
        checkItemRoomDescs _ = waitLocEvt

openDoor :: Monad m => RoomDir -> Pipe Event Event m LocationId
openDoor dir = (yield $ SendToServer ("смотреть " <> showt dir)) >> waitObstacleEvent >> return (LocationId 1)
  where waitObstacleEvent = await >>= checkObstacleEvent
        checkObstacleEvent (ServerEvent (ObstacleEvent _ obstacle)) = yield $ SendToServer ("открыть " <> obstacle <> " " <> showt dir)
        checkObstacleEvent (ServerEvent (UnknownServerEvent "")) = return ()
        checkObstacleEvent (ServerEvent (GlanceEvent _ _ _)) = return ()
        checkObstacleEvent _ = waitObstacleEvent
