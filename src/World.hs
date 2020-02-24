{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module World ( locsByRegex
             , showLocs
             , loadWorld
             , parseServerEvents
             , travelAction
             , zoneMap
             , loadServerEvents
             , serverLogEventsProducer
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
import qualified Data.List as L

data World = World { _worldMap :: WorldMap
                   , _locations :: Set Location
                   , _directions :: Directions
                   , _itemsOnMap :: Map ServerEvent (Map LocationId Int)
                   , _obstaclesOnMap :: Map (LocationId, RoomDir) Text
                   , _itemStats :: [ItemStats]
                   , _mobsDiscovered :: Map (ObjRef Mob InRoomDesc) (Map LocationId Int)
                   , _mobStats :: [Mob]
                   , _questActions :: Map (LocationId, LocationId) [Event]
                   , _mobsData :: Map Text (ObjCases Mob)
                   }

data Direction = Direction { locIdFrom :: LocIdFrom
                           , locIdTo :: LocIdTo
                           , trigger :: Trigger
                           }
                           deriving (Eq, Ord, Generic, Show)

type LocIdFrom = LocationId
type LocIdTo = LocationId
type Trigger = Text
type WorldMap = Gr () Int
type Directions = Map (LocationId, LocationId) RoomDir

unwrapRight :: Either ParsingError ServerEvent -> ServerEvent
unwrapRight (Right val) = val

accDirections :: Map (LocationId, LocationId) RoomDir -> [ServerEvent] -> Map (LocationId, LocationId) RoomDir
accDirections directions pair =
  let updateWorld locFrom locTo dir
        | locFrom == locTo = directions
        | otherwise = insertOpposite $ insertAhead directions
        where insertAhead = M.insert ((locFrom^.locationId), (locTo^.locationId)) $ parseDir dir
              insertOpposite = M.insert ((locTo^.locationId), (locFrom^.locationId)) $ oppositeDir $ parseDir dir
   in case pair of [LocationEvent locTo _ _ _, MoveEvent dir, LocationEvent locFrom _ _ _] -> updateWorld locFrom locTo dir
                   _ -> directions

parseDir :: Text -> RoomDir
parseDir "вверх" = Up
parseDir "вниз" = Down
parseDir "север" = North
parseDir "юг" = South
parseDir "запад" = West
parseDir "восток" = East

oppositeDir :: RoomDir -> RoomDir
oppositeDir Up = Down
oppositeDir Down = Up
oppositeDir North = South
oppositeDir South = North
oppositeDir West = East
oppositeDir East = West

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

extractObjects :: Monad m => (ServerEvent -> [ObjRef a InRoomDesc]) -> Pipe ServerEvent [ObjRef a InRoomDesc] m ()
extractObjects getter = PP.filter isLocationEvent >-> PP.map getter

extractDiscovered :: (Monad m, Ord a) => (ServerEvent -> [a]) -> Producer ServerEvent m () -> m (Map a (Map LocationId Int))
extractDiscovered entityExtractor producer = PP.fold toMap M.empty identity (producer >-> PP.filter isLocationEvent)
  where toMap acc evt@(LocationEvent (Location locId _) _ mobs _) = F.foldl (insertMob locId) acc (entityExtractor evt)
        insertMob locId acc mob = M.alter (updateCount locId) mob acc
        updateCount locId Nothing = Just (M.insert locId 1 M.empty)
        updateCount locId (Just locToCountMap) = Just (M.alter plusOne locId locToCountMap)
        plusOne Nothing = Just 1
        plusOne (Just x) = Just (x + 1)

discoverItems :: (Monad m) => Producer ServerEvent m () -> m (Map ServerEvent (Map LocationId Int))
discoverItems producer = foldToMap (producer >-> filterMapDiscoveries >-> scanEvtWithLoc Nothing)
  where filterMapDiscoveries = forever $ await >>= \case
              evt@(LocationEvent (Location locId _) items _ _) -> yield evt >> mapM_ (yield . ItemInTheRoom) items
              evt@LootItem{} -> yield evt
              evt@TakeFromContainer{} -> yield evt
              evt@MobGaveYouItem{} -> yield evt
              _ ->  return ()
        scanEvtWithLoc maybeLocId = await >>= \case (LocationEvent (Location locId _) _ _ _) -> scanEvtWithLoc (Just locId)
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
extractLocs serverEvtProducer = PP.fold toSet S.empty identity $ locations
  where toSet acc item = S.insert item acc
        locations = serverEvtProducer >-> PP.filter isLocationEvent >-> PP.map _location

extractItemStats :: Monad m => Producer ServerEvent m () -> Producer ItemStats m ()
extractItemStats serverEvtProducer = serverEvtProducer >-> PP.filter isItemStatsEvent >-> PP.map (\(ItemStatsEvent item) -> item)

extractDirections :: Monad m => Producer ServerEvent m () -> m (Map (LocationId, LocationId) RoomDir)
extractDirections serverEvents = PP.fold accDirections M.empty identity locationsAndMovesTriples
  where locationsAndMoves = serverEvents >-> PP.filter (\evt -> isLocationEvent evt || isMoveEvent evt)
        locationsAndMovesTriples = (locationsAndMoves >-> PP.scan toTriples [] identity
                                                      >-> PP.filter mappableMove)
                                                                                      where toTriples acc event
                                                                                              | length acc < 3 = event : acc
                                                                                              | otherwise = event : take 2 acc

listFilesIn :: FilePath -> IO [FilePath]
listFilesIn dir = ((dir ++ ) <$>) <$> listDirectory dir

serverLogEventsProducer :: Producer ServerEvent IO ()
serverLogEventsProducer = combineStreams =<< liftIO logFiles
  where logFiles = getCurrentDirectory >>= \currentDir -> listFilesIn (currentDir ++ "/" ++ serverLogDir)
        combineStreams = F.foldl' (\evtPipe file -> evtPipe >> logfileEvtStream file) (return ())
        logfileEvtStream file = PP.dropWhile notLocation <-< (parseServerEvents . loadServerEvents $ file)
        isLocation LocationEvent{} = True
        isLocation _ = False
        notLocation = not . isLocation

obstaclesOnMap :: IO (Map (LocationId, RoomDir) Text)
obstaclesOnMap = fmap M.fromList $ PP.toListM $ PP.map toMapItems <-< PP.filter isLocationThenObstacle <-< PP.zip obstacleEvents (PP.drop 1 <-< obstacleEvents)
  where isObstacleEvt ObstacleEvent{} = True
        isObstacleEvt LocationEvent{} = True
        isObstacleEvt _ = False
        obstacleEvents = PP.filter isObstacleEvt <-< serverLogEventsProducer
        isLocationThenObstacle (LocationEvent{}, ObstacleEvent{}) = True
        isLocationThenObstacle _ = False
        toMapItems (locEvt, (ObstacleEvent dir obstacle)) = ((_locationId . _location $ locEvt, dir), obstacle)


nubList :: Ord a => [a] -> [a]
nubList = S.elems . S.fromList

mobNominatives :: IO [ObjRef Mob Nominative]
mobNominatives = nubList <$> loadMobs
  where isFightPrompt FightPromptEvent{} = True
        isFightPrompt _ = False
        toPair (FightPromptEvent _ target) = target
        loadMobs = PP.toListM $ PP.map toPair <-< PP.filter isFightPrompt <-< serverLogEventsProducer

mobRoomDescs :: IO [MobRoomDesc]
mobRoomDescs = nubList <$> loadMobRoomDescs
  where loadMobRoomDescs = PP.toListM $ PP.concat <-< extractObjects _mobs <-< serverLogEventsProducer

type NominativeWords =  [Text]

loadWorld :: FilePath -> Map Text (ObjCases Mob) -> IO World
loadWorld currentDir customMobProperties = do
  serverLogFiles <- listFilesIn (currentDir ++ "/" ++ serverLogDir)
  evtLogFiles <- listFilesIn (currentDir ++ "/" ++ evtLogDir)
  directions <- extractDirections serverLogEventsProducer
  locations <- (extractLocs . parseServerEvents . loadLogs) serverLogFiles
  itemsStats <- PP.toListM $ (extractItemStats . parseServerEvents . loadLogs) serverLogFiles
  itemsOnMap <- (discoverItems . parseServerEvents . loadLogs) serverLogFiles
  mobsOnMap <- ((extractDiscovered _mobs) . parseServerEvents . loadLogs) serverLogFiles
  questActions <- (obstacleActions . binEvtLogParser . loadLogs) evtLogFiles
  obstaclesOnMap <- obstaclesOnMap
  mobsData <- mobsData
  let worldMap = buildMap locations directions
   in return World { _worldMap = worldMap
                   , _locations = locations
                   , _directions = directions
                   , _itemsOnMap = itemsOnMap
                   , _obstaclesOnMap = obstaclesOnMap
                   , _itemStats = itemsStats
                   , _mobsDiscovered = mobsOnMap
                   , _mobStats = []
                   , _questActions = questActions
                   , _mobsData = M.unionWith M.union customMobProperties mobsData
                   }

mobsData :: IO (Map Text (ObjCases Mob))
mobsData = mobsWithTargetFlag
  where mobsWithTargetFlag = M.unionWith (M.union) <$> mobs <*> (fmap (M.insert Target "true") <$> targetsByInRoomDescs)
        targetsByInRoomDescs = mobsByNominatives >>= \mbn -> targets >>= \t -> (return . groupByCase InRoomDesc . catMaybes $ (\trg -> M.lookup trg mbn) <$> t)
        mobsByNominatives = groupByCase Nominative . M.elems <$> mobs
        mobs = (\mobCases allRoomDescs -> M.unionWith (M.union) allRoomDescs mobCases) <$> mobCasesByInRoomDesc <*> allInRoomDescs
        targets = S.toList . S.fromList <$> PP.toListM (PP.map unObjRef <-< PP.map (\(FightPromptEvent _ target) -> target) <-< PP.filter isFightPromptEvent <-< serverLogEventsProducer)
        allInRoomDescs = groupByCase InRoomDesc . fmap (M.singleton InRoomDesc) . S.toList . S.fromList <$> PP.toListM (PP.map unObjRef <-< PP.concat <-< PP.map _mobs <-< PP.filter isLocationEvent <-< serverLogEventsProducer)
        mobCasesByInRoomDesc = fmap (groupByCase InRoomDesc) $ PP.toListM $ PP.map windowToCases <-< PP.filter allCasesWindow <-< scanWindow 7 <-< PP.filter isCheckCaseEvt <-< serverLogEventsProducer
        groupByCase c = M.fromList . catMaybes . fmap (\mobCases -> M.lookup c mobCases >>= \cs -> return (cs, mobCases))
        defaultAlias = T.intercalate "." . T.words
        allCasesWindow [prep, instr, dat, acc, gen, nom, locEvt] = locWithOneMob locEvt
                                                            && isCheckNominative nom
                                                            && isCheckGenitive gen
                                                            && isCheckAccusative acc
                                                            && isCheckDative dat
                                                            && isCheckInstrumental instr
                                                            && isCheckPrepositional prep
        allCasesWindow _ = False
        locWithOneMob (LocationEvent _ _ [mob] _) = True
        locWithOneMob _ = False
        isCheckCaseEvt evt = isLocationEvent evt || isCaseEvt evt
        isCaseEvt evt = isCheckNominative evt || isCheckGenitive evt || isCheckAccusative evt || isCheckDative evt || isCheckInstrumental evt || isCheckPrepositional evt
        windowToCases :: [ServerEvent] -> Map ObjCase Text
        windowToCases [CheckPrepositional prep, CheckInstrumental instr , CheckDative dat , CheckAccusative acc , CheckGenitive gen , CheckNominative nom , LocationEvent _ _ [mobInRoomDesc] _ ] = M.insert InRoomDesc (unObjRef mobInRoomDesc) . M.insert Nominative (unObjRef nom) . M.insert Genitive (unObjRef gen) . M.insert Accusative (unObjRef acc) . M.insert Dative (unObjRef dat) . M.insert Instrumental (unObjRef instr) . M.insert Prepositional (unObjRef prep) . M.insert Alias (defaultAlias . unObjRef $ nom) $ M.empty
        scanWindow n = PP.scan toWindow [] identity
          where toWindow acc event
                  | length acc < n = event : acc
                  | otherwise = event : take (n - 1) acc

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
  where onEndOrError Right{} = return ()
        onEndOrError (Left (err, producer)) = (liftIO $ print "error when parsing") >> (yield $ ParseError $ errDesc err)
        errDesc (ParsingError ctxts msg) = "error: " <> C8.pack msg <> C8.pack (concat ctxts) <> "\n"

buildMap :: Set Location -> Directions -> Gr () Int
buildMap locations directions = mkGraph nodes edges
  where edges = concat . fmap (\d -> [aheadEdge d, reverseEdge d]) . fmap (\(LocationId l, LocationId r) -> (l, r)) $ M.keys directions
        nodes = (\(Location (LocationId locId) _) -> (locId, ())) <$> (S.toList locations)
        aheadEdge (fromId, toId) = (fromId, toId, 1)
        reverseEdge (fromId, toId) = (toId, fromId, 1)

zoneMap :: World -> Int -> Gr () Int
zoneMap world anyZoneLocId = mkGraph nodes edges
  where edges = concat . fmap (\d -> [aheadEdge d, reverseEdge d]) . filterDirInZone . fmap (\(LocationId l, LocationId r) -> (l, r)) $ M.keys directions
        nodes = fmap (\n -> (n, ())) . concat . fmap (\(l, r, _) -> l : r : []) $ edges
        questActionVertexes ((LocationId fromId), (LocationId toId)) = (fromId, toId)
        aheadEdge (fromId, toId) = (fromId, toId, 1)
        reverseEdge (fromId, toId) = (toId, fromId, 1)
        filterDirInZone = filter (\(l, r) -> isInZone l && isInZone r)
        isInZone v = v - (mod v 100) == anyZoneLocId
        directions = _directions world

travelActions :: Monad m => Map (LocationId, LocationId) (Pipe Event Event m ServerEvent)
travelActions = M.fromList [ ((LocationId 5104, LocationId 5117), setupLadder)
                           , ((LocationId 5052, LocationId 4064), payOldGipsy)
                           , ((LocationId 4064, LocationId 5052), payYoungGipsy)
                           ]

{-
directionActions :: Monad m => World -> Map (LocationId, LocationId) RoomDir
directionActions world = M.fromList $ directionVertexes <$> (S.toList $ _directions world)
  where directionVertexes (Direction from to dir) = ((from, to), dir)
        movePipe dir = await >>= \case PulseEvent -> yield (SendToServer dir) >> waitLocation
                                       evt -> yield evt >> movePipe dir
        waitLocation = await >>= \evt -> yield evt >> case evt of (ServerEvent locEvt@LocationEvent{}) -> return locEvt
                                                                  _ -> waitLocation
-}

openObstacle :: MonadIO m => World -> ServerEvent -> RoomDir -> Pipe Event Event m ()
openObstacle world locEvt@LocationEvent{} dir = if L.elem (ClosedExit dir) (_exits locEvt)
                                             then findObstacleName >>= removeObstacle
                                             else return ()
  where glanceDirection = await >>= \case PulseEvent -> yield (SendToServer $ "смотреть " <> showt dir) >> return ()
                                          evt -> yield evt >> glanceDirection
        awaitObstacle = await >>= \case (ServerEvent (ObstacleEvent _ obstacle)) -> return obstacle
                                        evt -> yield evt >> awaitObstacle
        findObstacleName = case M.lookup (locId, dir) obstaclesOnMap of (Just obstacleName) -> return obstacleName
                                                                        Nothing -> glanceDirection >> awaitObstacle
        locId = _locationId . _location $ locEvt
        obstaclesOnMap = _obstaclesOnMap world
        removeObstacle obstacle = await >>= \case PulseEvent -> yield (SendToServer $ "открыть " <> obstacle <> " " <> showt dir)
                                                  evt -> yield evt >> removeObstacle obstacle

travelAction :: MonadIO m => World -> ServerEvent -> LocationId -> Pipe Event Event m ServerEvent
travelAction world fromLocEvt to = case M.lookup (from, to) (_directions world) of
                                          Nothing -> return fromLocEvt
                                          (Just dir) -> openObstacle world fromLocEvt dir >> movePipe dir
  where from = _locationId $ _location fromLocEvt
        movePipe dir = await >>= \case PulseEvent -> yield (SendToServer . showt $ dir) >> waitLocation
                                       evt -> yield evt >> movePipe dir
        waitLocation = await >>= \evt -> yield evt >> case evt of (ServerEvent locEvt@LocationEvent{}) -> return locEvt
                                                                  _ -> waitLocation

payOldGipsy :: Monad m => Pipe Event Event m ServerEvent
payOldGipsy = move
  where move = await >>= \case PulseEvent -> (yield $ SendToServer $ "дать 1 кун цыган") >> waitLocation
                               evt -> yield evt >> move
        waitLocation = await >>= \evt -> yield evt >> case evt of (ServerEvent locEvt@LocationEvent{}) -> return locEvt
                                                                  _ -> waitLocation

payYoungGipsy :: Monad m => Pipe Event Event m ServerEvent
payYoungGipsy = move
  where move = await >>= \case PulseEvent -> (yield $ SendToServer $ "дать 1 кун цыган") >> waitLocation
                               evt -> yield evt >> move
        waitLocation = await >>= \evt -> yield evt >> case evt of (ServerEvent locEvt@LocationEvent{}) -> return locEvt
                                                                  _ -> waitLocation

setupLadder :: Monad m => Pipe Event Event m ServerEvent
setupLadder = (yield $ SendToServer "смотреть") >> waitLocEvt
  where waitLocEvt = await >>= \evt -> yield evt >> checkItemRoomDescs evt
        checkItemRoomDescs (ServerEvent locEvt@LocationEvent{}) = if elem (ObjRef "На полу лежит лестница.") (_objects locEvt)
                                                                     then yield (SendToServer "приставить лестница") >> return locEvt
                                                                     else return locEvt
        checkItemRoomDescs _ = waitLocEvt
