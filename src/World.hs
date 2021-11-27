{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module World ( locsByRegex
             , mobsData
             , nominativeToEverAttacked
             , inRoomDescToMobCase
             , groupByCase
             , loadLogs
             , listFilesIn
             , extractLocs
             , extractDirections
             , showLocs
             , loadWorld
             , parseServerEvents
             , travelAction
             , zoneMap
             , loadServerEvents
             , serverLogEventsProducer
             , discoverItems
             , binarizeServerLog
             , parseEvtLog
             , scanFromTargetEvent
             , World(..)
             , Direction(..)
             , Trigger(..)
             , WorldMap
             , dropLoopsFromPath
             ) where

import Protolude hiding (Location, runStateT, Down, yield, to)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import ServerInputParser
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph hiding ((&))
import System.IO (hClose, hFlush)
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as PP
import Pipes.Attoparsec
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PBS
import qualified Pipes.Binary as PB
import Pipes.Binary (DecodingError)
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
import Data.Binary
import Data.Binary.Get

data World = World { _worldMap :: WorldMap
                   , _locationEvents :: Set Location
                   , _directions :: Directions
                   , _itemsOnMap :: Map ServerEvent (Map LocationId Int)
                   , _obstaclesOnMap :: Map (LocationId, RoomDir) Text
                   , _itemStats :: [ItemStats]
                   , _mobsDiscovered :: Map (ObjRef Mob InRoomDesc) (Map LocationId Int)
                   , _mobStats :: [MobStats]
                   , _questActions :: Map (LocationId, LocationId) [Event]
                   , _inRoomDescToMob :: Map (ObjRef Mob InRoomDesc) MobStats
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
  where locs = _locationEvents world

loadServerEvents :: FilePath -> Producer ByteString IO ()
loadServerEvents file = openfile >>= \h -> PBS.fromHandle h >> closefile h
  where openfile = lift $ openFile file ReadMode
        closefile h = lift $ hClose h

binEvtLogParser :: Producer ByteString IO () -> Producer Event IO ()
binEvtLogParser bsp = parseEventLogProducer =<< lift (PBS.toLazyM bsp) >-> PP.filter filterTravelActions

loadLogs :: [FilePath] -> Producer ByteString IO ()
loadLogs files = F.foldl' (\evtPipe file -> evtPipe >> loadServerEvents file) (return ()) files

extractObjects :: Monad m => (ServerEvent -> a) -> Pipe ServerEvent a m ()
extractObjects getter = PP.filter (has _LocationEvent) >-> PP.map getter

extractDiscovered :: (Monad m) => Producer ServerEvent m () -> m (Map (ObjRef Mob InRoomDesc) (Map LocationId Int))
extractDiscovered producer = PP.fold toMap M.empty identity (PP.filter (has _LocationEvent) <-< producer)
  where toMap acc evt@(LocationEvent (Location locId _) _ mobs _) = F.foldl (insertMob locId) acc (_mobs evt)
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
extractLocs serverEvtProducer = PP.fold toSet S.empty identity $ locationEvents
  where toSet acc item = S.insert item acc
        locationEvents = serverEvtProducer >-> PP.filter (has _LocationEvent) >-> PP.map _location

extractItemStats :: Monad m => Producer ServerEvent m () -> Producer ItemStats m ()
extractItemStats serverEvtProducer = serverEvtProducer >-> PP.filter (has _ItemStatsEvent) >-> PP.map (\(ItemStatsEvent item) -> item)

extractDirections :: Monad m => Producer ServerEvent m () -> m (Map (LocationId, LocationId) RoomDir)
extractDirections serverEvents = PP.fold accDirections M.empty identity locationEventsAndMovesTriples
  where locationEventsAndMoves = serverEvents >-> PP.filter (\evt -> has _LocationEvent evt || has _MoveEvent evt || has _CodepagePrompt evt)
        locationEventsAndMovesTriples = (locationEventsAndMoves >-> PP.scan toTriples [] identity
                                                      >-> PP.filter mappableMove)
                                                                                      where toTriples acc event
                                                                                              | length acc < 3 = event : acc
                                                                                              | otherwise = event : take 2 acc

listFilesIn :: FilePath -> IO [FilePath]
listFilesIn dir = ((dir ++ ) <$>) <$> listDirectory dir

serverLogEventsProducer :: Producer ServerEvent IO ()
serverLogEventsProducer = combineStreams =<< liftIO logFiles
  where logFiles = getCurrentDirectory >>= \currentDir -> listFilesIn (currentDir ++ "/" ++ serverLogDir)
        combineStreams = F.foldl' (\evtPipe file -> evtPipe >> logfileEvtStream file >> yield EndOfLogEvent) (return ())
        logfileEvtStream file = PP.dropWhile (hasn't _LocationEvent ) <-< (parseServerEvents . loadServerEvents $ file)

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
  where toPair (FightPromptEvent _ target) = target
        loadMobs = PP.toListM $ PP.map toPair <-< PP.filter (has _FightPromptEvent) <-< serverLogEventsProducer

mobRoomDescs :: IO [ObjRef Mob InRoomDesc]
mobRoomDescs = nubList <$> loadMobRoomDescs
  where loadMobRoomDescs = PP.toListM $ PP.concat <-< extractObjects _mobs <-< serverLogEventsProducer

type NominativeWords =  [Text]

loadWorld :: FilePath -> Map (ObjRef Mob InRoomDesc) MobStats -> IO World
loadWorld currentDir customMobProperties = do
  serverLogFiles <- listFilesIn (currentDir ++ "/" ++ serverLogDir)
  evtLogFiles <- listFilesIn (currentDir ++ "/" ++ evtLogDir)
  directions <- (extractDirections . parseServerEvents . loadLogs) serverLogFiles
  locationEvents <- (extractLocs . parseServerEvents . loadLogs) serverLogFiles
  itemsStats <- pure []--PP.toListM $ (extractItemStats . parseServerEvents . loadLogs) serverLogFiles
  itemsOnMap <- pure M.empty--(discoverItems . parseServerEvents . loadLogs) serverLogFiles
  mobsOnMap <- pure M.empty--(extractDiscovered . parseServerEvents . loadLogs) serverLogFiles
  questActions <- pure M.empty--(obstacleActions . binEvtLogParser . loadLogs) evtLogFiles
  obstaclesOnMap <- pure M.empty--obstaclesOnMap
  mobsData <- mobsData
  let worldMap = buildMap locationEvents directions
   in return World { _worldMap = worldMap
                   , _locationEvents = locationEvents
                   , _directions = directions
                   , _itemsOnMap = itemsOnMap
                   , _obstaclesOnMap = obstaclesOnMap
                   , _itemStats = itemsStats
                   , _mobsDiscovered = mobsOnMap
                   , _mobStats = []
                   , _questActions = questActions
                   , _inRoomDescToMob = M.unionWith (<>) customMobProperties mobsData
                   }

archiveToServerEvents :: Producer ServerEvent IO ()
archiveToServerEvents = (liftIO $ listFilesIn ("." ++ "/" ++ serverLogDir)) >>= (parseServerEvents . loadLogs)

groupByCase :: (NameCases Mob -> Maybe (ObjRef Mob b)) -> [MobStats] -> Map (ObjRef Mob b) MobStats
groupByCase getter = M.fromList . fmap (\stats -> (fromJust . getter . _nameCases $ stats, stats)) . L.filter (isJust . getter . _nameCases)

regroupTo :: (NameCases Mob -> Maybe (ObjRef Mob b)) -> Map (ObjRef Mob a) MobStats -> Map (ObjRef Mob b) MobStats
regroupTo getter = groupByCase getter . fmap snd . M.toList
  
nominativeToEverAttacked :: IO (Map (ObjRef Mob Nominative) MobStats)
nominativeToEverAttacked = groupByCase _nominative . fmap (\ref -> mempty & everAttacked ?~ EverAttacked True & nameCases . nominative ?~ ref) <$> (PP.toListM $ PP.map _target <-< PP.filter (has _FightPromptEvent) <-< archiveToServerEvents)

inRoomDescToMobCase :: IO (Map (ObjRef Mob InRoomDesc) MobStats)
inRoomDescToMobCase =
  groupByCase _inRoomDesc <$>
  (PP.toListM $
   PP.map (\w -> nameCases .~ windowToCases w $ mempty) <-<
   PP.filter allCasesWindow <-<
   scanWindow 7 <-<
   PP.filter isCheckCaseEvt <-<
   archiveToServerEvents)
  where
    windowToCases :: [ServerEvent] -> NameCases Mob
    windowToCases [CheckPrepositional prep, CheckInstrumental instr, CheckDative dat, CheckAccusative acc, CheckGenitive gen, CheckNominative nom, LocationEvent _ _ [mob] _] =
      NameCases
        { _inRoomDesc = Just mob
        , _nominative = Just nom
        , _genitive = Just gen
        , _accusative = Just acc
        , _dative = Just dat
        , _instrumental = Just instr
        , _prepositional = Just prep
        , _alias = Just . ObjRef . defaultAlias . unObjRef $ nom
        }
    allCasesWindow [prep, instr, dat, acc, gen, nom, locEvt] =
      locWithOneMob locEvt &&
      has _CheckNominative nom &&
      has _CheckGenitive gen &&
      has _CheckAccusative acc &&
      has _CheckDative dat &&
      has _CheckInstrumental instr && has _CheckPrepositional prep
    allCasesWindow _ = False
    scanWindow n = PP.scan toWindow [] identity
          where toWindow acc event
                  | length acc < n = event : acc
                  | otherwise = event : take (n - 1) acc
    isCheckCaseEvt evt = has _LocationEvent evt || isCaseEvt evt
    isCaseEvt evt = has _CheckNominative evt || has _CheckGenitive evt || has _CheckAccusative evt || has _CheckDative evt || has _CheckInstrumental evt || has _CheckPrepositional evt
    defaultAlias = T.intercalate "." . T.words
    locWithOneMob (LocationEvent _ _ [mob] _) = True
    locWithOneMob _ = False

mobsData :: IO (Map (ObjRef Mob InRoomDesc) MobStats)
mobsData =
  regroupTo _inRoomDesc <$>
  (M.unionWith (<>) <$> nominativeToMobCase <*> nominativeToEverAttacked)
  where
    nominativeToMobCase = regroupTo _nominative <$> inRoomDescToMobCase

printWorldStats :: World -> Producer Event IO ()
printWorldStats world = yield $ ConsoleOutput worldStats
  where worldStats = encodeUtf8 $ locationEventsStats <> directionsStats <> items <> itemsStats <> mobs
        locationEventsStats = (show . length . _locationEvents) world <> " локаций найдено\n"
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
buildMap locationEvents directions = mkGraph nodes edges
  where edges = concat . fmap (\d -> [aheadEdge d, reverseEdge d]) . fmap (\(LocationId l, LocationId r) -> (l, r)) $ M.keys directions
        nodes = (\(Location (LocationId locId) _) -> (locId, ())) <$> (S.toList locationEvents)
        aheadEdge (fromId, toId) = (fromId, toId, 1)
        reverseEdge (fromId, toId) = (toId, fromId, 1)

zoneMap :: World -> Int -> Gr () Int
zoneMap world anyZoneLocId = mkGraph nodes edges
  where
    edges =
      L.nub . mconcat . fmap (\(LocationId fromId, LocationId toId) -> [(fromId, toId, 1), (toId, fromId, 1)]) .
      L.filter dirInZone . M.keys . _directions $
      world
    nodes =
      fmap (\n -> (n, ())) . L.nub . mconcat . fmap (\(l, r, _) -> [l, r]) $ edges
    dirInZone (LocationId lid, LocationId rid) = isInZone lid && isInZone rid
    isInZone locId = (div locId 100) == (div anyZoneLocId 100)

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

travelAction :: MonadIO m => World -> LocationId -> LocationId -> Pipe Event Event m ()
travelAction world from to = case M.lookup (from, to) (_directions world) of
                                          Nothing -> return ()
                                          (Just dir) -> yield (SendOnPulse 5 . showt $ dir)

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

binarizeServerLog :: IO ()
binarizeServerLog =
  PP.toListM (serverLogEventsProducer >-> PP.map ServerEvent) >>= \events ->
    runEffect
      ((Pipes.each $ L.reverse events) >-> Pipes.for cat PB.encode >-> toEvtLog)
  where
    toEvtLog =
      (liftIO $ openFile "evt.log" WriteMode) >>= \h ->
        PBS.toHandle h >> (liftIO $ hFlush h) >> (liftIO $ hClose h)

parseEvtLog :: IO (Either (DecodingError, Producer ByteString IO ()) ())
parseEvtLog = runEffect $ producer ^. PB.decoded >-> printEvents
  where
    producer =
      (lift $ openFile "evt.log" ReadMode) >>= \h ->
        PBS.fromHandle h >>
        (lift $ hClose h)

scanFromTargetEvent :: IO ()
scanFromTargetEvent =
  runEffect $
  (producer ^. PB.decoded >> pure ()) >-> PP.scan onEvent Nothing identity >->
  PP.filter (\v -> fmap fst v == Just True) >->
  PP.map (snd . fromJust) >->
  printEvents
  where
    onEvent _ item@(ServerEvent (LocationEvent (Location (LocationId 5119) _) _ _ _)) = Just (True, item)
    onEvent _ item@(ServerEvent EndOfLogEvent) = Just (False, item)
    onEvent acc item = fmap (\(flag, _) -> (flag, item)) acc
    producer =
      (lift $ openFile "evt.log" ReadMode) >>= \h ->
        PBS.fromHandle h >> (lift $ hClose h)

dropLoopsFromPath :: IO (Map LocationId Int)
dropLoopsFromPath = PP.fold onEvent M.empty identity indexedLocations
  where
    indexedLocations = PP.zip (Pipes.each [1..]) locationEvents
    locationEvents = logEvents >-> PP.filter (has (_ServerEvent . _LocationEvent))
    onEvent acc item@(i, ServerEvent (LocationEvent (Location locId _) _ _ _)) = M.insert locId i acc
    logEvents = producer ^. PB.decoded >> pure ()
    producer =
      (lift $ openFile "evt.log" ReadMode) >>= \h ->
        PBS.fromHandle h >> (lift $ hClose h)
