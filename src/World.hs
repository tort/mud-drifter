{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module World where

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
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Text.Pretty.Simple
import Text.Shakespeare.Text
import Data.List.Split

data World = World { _worldMap :: WorldMap
                   , _locationEvents :: Map Int (ZonedLocation)
                   , _directions :: Directions
                   , _obstaclesOnMap :: Map (Int, RoomDir) Text
                   , _questActions :: Map (Int, Int) [Event]
                   , _itemsOnMap :: Map ServerEvent (Map (Int) Int)
                   , _itemStats :: [ItemStats]
                   , _inRoomDescToMobOnMap :: Map (ObjRef Mob InRoomDesc) (Map (Int) Int)
                   , _inRoomDescToMob :: Map (ObjRef Mob InRoomDesc) MobStats
                   , _nominativeToMob :: Map (ObjRef Mob Nominative) MobStats
                   }

data Direction = Direction { locIdFrom :: LocIdFrom
                           , locIdTo :: LocIdTo
                           , trigger :: Trigger
                           }
                           deriving (Eq, Ord, Generic, Show)

type LocIdFrom = Int
type LocIdTo = Int
type Trigger = Text
type WorldMap = Gr () Int
type Directions = Map (Int, Int) RoomDir

unwrapRight :: Either ParsingError ServerEvent -> ServerEvent
unwrapRight (Right val) = val

accDirections :: Map (Int, Int) RoomDir -> [ServerEvent] -> Map (Int, Int) RoomDir
accDirections directions pair =
  let updateWorld locFrom locTo dir
        | locFrom == locTo = directions
        | otherwise = insertOpposite $ insertAhead directions
        where insertAhead = M.insert ((locFrom^.locationId), (locTo^.locationId)) $ parseDir dir
              insertOpposite = M.insert ((locTo^.locationId), (locFrom^.locationId)) $ oppositeDir $ parseDir dir
   in case pair of [LocationEvent locTo _ _ _ _, MoveEvent dir, LocationEvent locFrom _ _ _ _] -> updateWorld locFrom locTo dir
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
        renderLoc node = (showt (node^.locationId) <> " " <> (node^.locationTitle)) :: Text
        addRet txt = T.snoc txt '\n'

locsByRegex :: World -> Text -> [Location]
locsByRegex world regex = S.toList $ S.filter (T.isInfixOf regex . T.toLower . (\l -> l^.locationTitle)) locs
  where locs =  S.fromList . fmap (_zloc) . M.elems $ (_locationEvents world)

loadServerEvents :: MonadIO m => FilePath -> Producer ByteString m ()
loadServerEvents file = openfile >>= \h -> PBS.fromHandle h >> closefile h
  where openfile = liftIO $ openFile file ReadMode
        closefile h = liftIO $ hClose h

loadLogs :: [FilePath] -> Producer ByteString IO ()
loadLogs files = F.foldl' (\evtPipe file -> evtPipe >> loadServerEvents file) (return ()) files

extractObjects :: Monad m => (ServerEvent -> a) -> Pipe ServerEvent a m ()
extractObjects getter = PP.filter (has _LocationEvent) >-> PP.map getter

isNominativeMob :: ObjRef Mob InRoomDesc -> Bool
isNominativeMob (ObjRef mob) = 
        (T.isSuffixOf " сражается с ВАМИ!") mob ||
        (T.isSuffixOf " отдыхает здесь.") mob ||
        (T.isSuffixOf " лежит здесь, при смерти.") mob ||
        (T.isSuffixOf " лежат здесь, при смерти.") mob ||
        (T.isSuffixOf " лежит здесь, в обмороке.") mob ||
        (T.isSuffixOf " лежат здесь, в обмороке.") mob

extractDiscovered :: (Monad m) => Producer ServerEvent m () -> m (Map (ObjRef Mob InRoomDesc) (Map (Int) Int))
extractDiscovered producer = PP.fold toMap M.empty identity (PP.filter (has _LocationEvent) <-< producer)
  where toMap acc evt@(LocationEvent (Location locId _) _ mobs _ _) = F.foldl (insertMob locId) acc ((L.filter (not . isNominativeMob) . _mobs) evt)
        insertMob locId acc mob = M.alter (updateCount locId) mob acc
        updateCount locId Nothing = Just (M.insert locId 1 M.empty)
        updateCount locId (Just locToCountMap) = Just (M.alter plusOne locId locToCountMap)
        plusOne Nothing = Just 1
        plusOne (Just x) = Just (x + 1)

discoverItems :: (Monad m) => Producer ServerEvent m () -> m (Map ServerEvent (Map (Int) Int))
discoverItems producer = foldToMap (producer >-> filterMapDiscoveries >-> scanEvtWithLoc Nothing)
  where filterMapDiscoveries = forever $ await >>= \case
              evt@(LocationEvent (Location locId _) items _ _ _) -> yield evt >> mapM_ (yield . ItemInTheRoom) items
              evt@LootItem{} -> yield evt
              evt@TakeFromContainer{} -> yield evt
              evt@MobGaveYouItem{} -> yield evt
              _ ->  return ()
        scanEvtWithLoc maybeLocId = await >>= \case (LocationEvent (Location locId _) _ _ _ _) -> scanEvtWithLoc (Just locId)
                                                    evt -> case maybeLocId of Nothing -> scanEvtWithLoc Nothing
                                                                              (Just locId) -> yield (evt, locId) >> scanEvtWithLoc maybeLocId
        foldToMap = PP.fold toMap M.empty identity
        toMap acc (evt, locId) = insertMob locId acc evt
        insertMob locId acc mob = M.alter (updateCount locId) mob acc
        updateCount locId Nothing = Just (M.insert locId 1 M.empty)
        updateCount locId (Just locToCountMap) = Just (M.alter plusOne locId locToCountMap)
        plusOne Nothing = Just 1
        plusOne (Just x) = Just (x + 1)


extractLocs :: Monad m => Producer ServerEvent m () -> m (Set (Location))
extractLocs serverEvtProducer = PP.fold toSet S.empty identity $ locationEvents
  where toSet acc item = S.insert item acc
        locationEvents = serverEvtProducer >-> PP.filter (has _LocationEvent) >-> PP.map _location

extractLocZones :: Monad m => Producer ServerEvent m () -> m (Map Int (ZonedLocation))
extractLocZones serverEvtProducer =
  PP.fold toMap M.empty identity $ locationEvents serverEvtProducer
  where
    toMap acc (Just l@(Location id title), Just v) = M.insert id (ZonedLocation l v) acc
    toMap acc (k, v) = acc
    locationEvents serverEvtProducer =
      serverEvtProducer >-> scanZone >-> PP.map (fromJust . fst) >->
      PP.map
        (liftA2
           (,)
           (preview (_LocationEvent . _1))
           (preview (_LocationEvent . _5 . traverse)))

extractItemStats :: Monad m => Producer ServerEvent m () -> Producer ItemStats m ()
extractItemStats serverEvtProducer = serverEvtProducer >-> PP.filter (has _ItemStatsEvent) >-> PP.map (\(ItemStatsEvent item) -> item)

extractDirections :: Monad m => Producer ServerEvent m () -> m (Map (Int, Int) RoomDir)
extractDirections serverEvents = PP.fold accDirections M.empty identity locationEventsAndMovesTriples
  where locationEventsAndMoves = serverEvents >-> PP.filter (\evt -> has _LocationEvent evt || has _MoveEvent evt || has _CodepagePrompt evt)
        locationEventsAndMovesTriples = (locationEventsAndMoves >-> PP.scan toTriples [] identity
                                                      >-> PP.filter mappableMove)
                                                                                      where toTriples acc event
                                                                                              | length acc < 3 = event : acc
                                                                                              | otherwise = event : take 2 acc

listFilesIn :: MonadIO m => FilePath -> m [FilePath]
listFilesIn dir = ((dir ++ ) <$>) <$> (liftIO . listDirectory) dir

serverLogEventsProducer :: MonadIO m => Producer ServerEvent m ()
serverLogEventsProducer = combineStreams =<< liftIO logFiles
  where logFiles = getCurrentDirectory >>= \currentDir -> listFilesIn (currentDir ++ "/" ++ serverLogDir)
        combineStreams = F.foldl' (\evtPipe file -> evtPipe >> logfileEvtStream file >> yield EndOfLogEvent) (pure ())
        logfileEvtStream file = PP.dropWhile (hasn't _LocationEvent ) <-< (parseServerEvents . loadServerEvents $ file)

obstaclesOnMap :: IO (Map (Int, RoomDir) Text)
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

cacheLocationsFile = "cache/locations.json"
cacheDirectionsFile ="cache/directions.json"
cacheMobsOnMapFile = "cache/mobs-on-map.json"
cacheMobsDataFile = "cache/mobs-data.json"
cacheMobAliasFile = "cache/mobs-aliases.json"

generateCache :: IO ()
generateCache =
  cacheLocations *> loadCachedLocations >>= \locations ->
    cacheDirections *> cacheMobsOnMap *> loadCachedMobsOnMap >>= \mobsOnMap ->
      cacheMobsAliases mobsOnMap *> loadCachedMobAliases >>= \mobsAliases ->
        cacheMobData mobsAliases locations

generateAliases :: IO ()
generateAliases =
  cacheMobsOnMap *> loadCachedMobsOnMap >>= \mobsOnMap ->
    cacheMobsAliases mobsOnMap

cacheLocations :: IO ()
cacheLocations =
  listFilesIn "archive/server-input-log/" >>=
  extractLocZones . parseServerEvents . loadLogs >>=
  LC8.writeFile cacheLocationsFile . encodePretty . toJSON

cacheMobData :: Map Text Text -> Map Int ZonedLocation -> IO ()
cacheMobData mobsAliases locations =
  mobsData mobsAliases locations >>= LC8.writeFile cacheMobsDataFile . encodePretty . toJSON

cacheDirections :: IO ()
cacheDirections =
  listFilesIn "archive/server-input-log/" >>=
  extractDirections . parseServerEvents . loadLogs >>=
  LC8.writeFile cacheDirectionsFile . encodePretty . toJSON

cacheMobsOnMap :: IO ()
cacheMobsOnMap =
  listFilesIn "archive/server-input-log/" >>=
  extractDiscovered . parseServerEvents . loadLogs >>=
  LC8.writeFile cacheMobsOnMapFile . encodePretty . toJSON

cacheMobsAliases :: Map (ObjRef 'Mob 'InRoomDesc) (Map Int Int) -> IO ()
cacheMobsAliases mobsOnMap =
  (M.union <$> loadCachedRawMobAliases <*> allMobs) >>= LC8.writeFile cacheMobAliasFile . encodePretty @[(Text , Maybe Text)] . L.sortBy (\l r -> compare (snd l) (snd r)) . M.toList
  where
    allMobs :: IO (Map Text (Maybe Text))
    allMobs = M.fromList . fmap ((, Nothing) . unObjRef) . M.keys <$> (pure mobsOnMap)

loadCachedMobData :: IO (Map (ObjRef Mob InRoomDesc) MobStats)
loadCachedMobData = fmap fromJust . decodeFileStrict $ cacheMobsDataFile

loadCachedLocations :: IO (Map Int ZonedLocation)
loadCachedLocations = fmap fromJust . decodeFileStrict $ cacheLocationsFile

loadCachedDirections :: IO (Map (Int, Int) RoomDir)
loadCachedDirections = fmap fromJust . decodeFileStrict $ cacheDirectionsFile

loadCachedMobsOnMap :: IO (Map (ObjRef Mob InRoomDesc) (Map (Int) Int))
loadCachedMobsOnMap = fmap fromJust . decodeFileStrict $ cacheMobsOnMapFile

loadCachedMobAliases :: IO (Map Text Text)
loadCachedMobAliases = M.map (fromJust) . M.filter isJust <$> loadCachedRawMobAliases

loadCachedRawMobAliases :: IO (Map Text (Maybe Text))
loadCachedRawMobAliases = fmap (M.fromList . fromJust) . decodeFileStrict @[(Text, Maybe Text)] $ cacheMobAliasFile

loadWorld :: FilePath -> Map (ObjRef Mob InRoomDesc) MobStats -> IO World
loadWorld currentDir customMobProperties = do
  serverLogFiles <- listFilesIn (currentDir ++ "/" ++ serverLogDir)
  evtLogFiles <- listFilesIn (currentDir ++ "/" ++ evtLogDir)
  directions <- loadCachedDirections
  locationEvents <- loadCachedLocations
  itemsStats <- pure []--PP.toListM $ (extractItemStats . parseServerEvents . loadLogs) serverLogFiles
  itemsOnMap <- pure M.empty--(discoverItems . parseServerEvents . loadLogs) serverLogFiles
  mobsOnMap <- loadCachedMobsOnMap
  questActions <- pure M.empty--(obstacleActions . binEvtLogParser . loadLogs) evtLogFiles
  obstaclesOnMap <- pure M.empty--obstaclesOnMap
  mobsData <- loadCachedMobData
  let worldMap = buildMap (S.fromList . fmap (_zloc) . M.elems $ locationEvents) ((M.keys (travelActions @IO)) <> (M.keys directions))
   in return World { _worldMap = worldMap
                   , _locationEvents = locationEvents
                   , _directions = directions
                   , _itemsOnMap = itemsOnMap
                   , _obstaclesOnMap = obstaclesOnMap
                   , _itemStats = itemsStats
                   , _inRoomDescToMobOnMap = mobsOnMap
                   , _questActions = questActions
                   , _inRoomDescToMob = mobsData
                   , _nominativeToMob = regroupTo _nominative mobsData
                   }

archiveToServerEvents :: Producer ServerEvent IO ()
archiveToServerEvents = (liftIO $ listFilesIn ("." ++ "/" ++ serverLogDir)) >>= (parseServerEvents . loadLogs)

groupByCase :: (NameCases Mob -> ObjRef Mob b) -> [MobStats] -> Map (ObjRef Mob b) MobStats
groupByCase getter = M.fromList . fmap (\stats -> (getter . _nameCases $ stats, stats))

regroupTo :: (NameCases Mob -> ObjRef Mob b) -> Map (ObjRef Mob a) MobStats -> Map (ObjRef Mob b) MobStats
regroupTo getter = groupByCase getter . fmap snd . M.toList

nominativeToEverAttacked :: IO [(ObjRef Mob Nominative, Text)]
nominativeToEverAttacked = 
  PP.toListM $
  PP.map fromJust <-<
  PP.filter isJust <-<
  PP.map (uncurry (liftA2 (,))) <-<
  PP.map (bimap getNominative identity) <-< scanZone <-< archiveToServerEvents
  where
    getNominative :: Maybe ServerEvent -> Maybe (ObjRef Mob Nominative)
    getNominative = preview (traverse . _FightPromptEvent . _2)

scanZoneEvent :: MonadIO m => Pipe Event (Maybe Event, Maybe Text) m ()
scanZoneEvent = PP.filter (\(evt, z) -> isJust evt && isJust z) <-< PP.scan action (Nothing, Nothing) identity
  where
    action :: (Maybe Event, Maybe Text) -> Event -> (Maybe Event, Maybe Text)
    action (_, _) evt@(ServerEvent (LocationEvent _ _ _ _ (Just zone))) = (Just evt, Just zone)
    action (_, prevZone) (ServerEvent evt@(LocationEvent _ _ _ _ Nothing)) = (Just . ServerEvent $ evt & zone .~ prevZone, prevZone)
    action (_, prevZone) e = (Just e, prevZone)

scanZone :: Monad m => Pipe ServerEvent (Maybe ServerEvent, Maybe Text) m ()
scanZone = PP.filter (\(evt, z) -> isJust evt && isJust z) <-< PP.scan action (Nothing, Nothing) identity
  where
    action :: (Maybe ServerEvent, Maybe Text) -> ServerEvent -> (Maybe ServerEvent, Maybe Text)
    action (_, _) evt@(LocationEvent _ _ _ _ (Just zone)) = (Just evt, Just zone)
    action (_, prevZone) evt@(LocationEvent _ _ _ _ Nothing) = (Just $ evt & zone .~ prevZone, prevZone)
    action (_, prevZone) e = (Just e, prevZone)

inRoomDescToMobCase :: Map Text Text -> Map Int ZonedLocation -> Set (ObjRef Mob Nominative, Text) -> IO (Map (ObjRef Mob InRoomDesc) MobStats)
inRoomDescToMobCase mobAliases locations everAttackedMobs =
  groupByCase _inRoomDesc <$>
  (PP.toListM $ PP.map (fromJust) <-< PP.filter (isJust) <-<
   PP.map windowToCases <-<
   scanWindow 7 <-<
   PP.filter isCheckCaseEvt <-<
   archiveToServerEvents)
  where
    windowToCases :: [ServerEvent] -> Maybe MobStats
    windowToCases [prep, instr, dat, acc, gen, nom, loc] =
      (loc ^? _LocationEvent . _1 . locationId >>= \locId -> locations ^? at locId . traversed . zone) >>= \zone ->
      MobStats <$>
      (NameCases <$> (loc ^. mobs . to singleMob) <*> (nom ^? _CheckNominative) <*>
       (gen ^? _CheckGenitive) <*>
       (acc ^? _CheckAccusative) <*>
       (dat ^? _CheckDative) <*>
       (instr ^? _CheckInstrumental) <*>
       (prep ^? _CheckPrepositional) <*>
       (loc ^. mobs . to singleMob >>= \m -> mobAliases ^? at (unObjRef m) . traversed . to ObjRef)) <*>
      (nom ^? _CheckNominative >>= \nom -> everAttackedMobs ^? contains (nom, zone)) <*>
      (Just zone)
    windowToCases _ = Nothing
    singleMob [] = Nothing
    singleMob [mob] = Just mob
    singleMob _ = Nothing
    scanWindow n = PP.scan toWindow [] identity
      where
        toWindow acc event
          | length acc < n = event : acc
          | otherwise = event : take (n - 1) acc
    isCheckCaseEvt evt = has _LocationEvent evt || isCaseEvt evt
    isCaseEvt evt =
      has _CheckNominative evt || has _CheckGenitive evt ||
      has _CheckAccusative evt ||
      has _CheckDative evt ||
      has _CheckInstrumental evt ||
      has _CheckPrepositional evt
    defaultAlias = T.intercalate "." . T.words

mobsData :: Map Text Text -> Map Int ZonedLocation -> IO (Map (ObjRef Mob InRoomDesc) MobStats)
mobsData mobsAliases locations = S.fromList <$> nominativeToEverAttacked >>= inRoomDescToMobCase mobsAliases locations

printWorldStats :: World -> Producer Event IO ()
printWorldStats world = yield $ ConsoleOutput worldStats
  where worldStats = encodeUtf8 $ locationEventsStats <> directionsStats <> items <> itemsStats <> mobs
        locationEventsStats = (show . length . _locationEvents) world <> " локаций найдено\n"
        directionsStats = (show . length . _directions) world <> " переходов между локациями\n"
        items = (show . length . _itemsOnMap) world <> " предметов найдено\n"
        itemsStats = (show . length . _itemStats) world <> " предметов опознано\n"
        mobs = (show . length . _inRoomDescToMobOnMap) world <> " мобов найдено\n"

parseServerEvents :: MonadIO m => Producer ByteString m () -> Producer ServerEvent m ()
parseServerEvents src = PA.parsed serverInputParser src >>= onEndOrError
  where onEndOrError Right{} = return ()
        onEndOrError (Left (err, producer)) = (liftIO $ print "error when parsing") >> (yield $ ParseError $ errDesc err)
        errDesc (ParsingError ctxts msg) = "error: " <> C8.pack msg <> C8.pack (concat ctxts) <> "\n"

buildMap :: Set Location -> [(Int, Int)] -> Gr () Int
buildMap locations directions = mkGraph nodes edges
  where edges = concat . fmap (\d -> [aheadEdge d, reverseEdge d]) . fmap (\(l, r) -> (l, r)) $ directions
        nodes = (\(Location (locId) _) -> (locId, ())) <$> (S.toList locations)
        aheadEdge (fromId, toId) = (fromId, toId, 1)
        reverseEdge (fromId, toId) = (toId, fromId, 1)

zoneMap :: World -> Int -> Gr () Int
zoneMap world anyZoneLocId = mkGraph nodes edges
  where
    edges =
      L.nub . mconcat . fmap (\(fromId, toId) -> [(fromId, toId, 1), (toId, fromId, 1)]) .
      L.filter dirInZone . M.keys . _directions $
      world
    nodes =
      fmap (\n -> (n, ())) . L.nub . mconcat . fmap (\(l, r, _) -> [l, r]) $ edges
    dirInZone (lid, rid) = isInZone lid && isInZone rid
    isInZone locId = (div locId 100) == (div anyZoneLocId 100)

openObstacle :: MonadIO m => World -> ServerEvent -> RoomDir -> Pipe Event Event m ()
openObstacle world locEvt@LocationEvent{} dir = if L.elem (ClosedExit dir) (_exits locEvt)
                                             then findObstacleName >>= removeObstacle
                                             else return ()
  where glanceDirection = await >>= \case PulseEvent -> yield (SendToServer $ "смотреть " <> genericShowt dir) >> return ()
                                          evt -> yield evt >> glanceDirection
        awaitObstacle = await >>= \case (ServerEvent (ObstacleEvent _ obstacle)) -> return obstacle
                                        evt -> yield evt >> awaitObstacle
        findObstacleName = case M.lookup (locId, dir) obstaclesOnMap of (Just obstacleName) -> return obstacleName
                                                                        Nothing -> glanceDirection >> awaitObstacle
        locId = _locationId . _location $ locEvt
        obstaclesOnMap = _obstaclesOnMap world
        removeObstacle obstacle = await >>= \case PulseEvent -> yield (SendToServer $ "открыть " <> obstacle <> " " <> genericShowt dir)
                                                  evt -> yield evt >> removeObstacle obstacle

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
    onEvent _ item@(ServerEvent (LocationEvent (Location (5119) _) _ _ _ _)) = Just (True, item)
    onEvent _ item@(ServerEvent EndOfLogEvent) = Just (False, item)
    onEvent acc item = fmap (\(flag, _) -> (flag, item)) acc
    producer =
      (lift $ openFile "evt.log" ReadMode) >>= \h ->
        PBS.fromHandle h >> (lift $ hClose h)

dropLoopsFromPath :: IO (Map (Int) Int)
dropLoopsFromPath = PP.fold onEvent M.empty identity indexedLocations
  where
    indexedLocations = PP.zip (Pipes.each [1..]) locationEvents
    locationEvents = logEvents >-> PP.filter (has (_ServerEvent . _LocationEvent))
    onEvent acc item@(i, ServerEvent (LocationEvent (Location locId _) _ _ _ _)) = M.insert locId i acc
    logEvents = producer ^. PB.decoded >> pure ()
    producer =
      (lift $ openFile "evt.log" ReadMode) >>= \h ->
        PBS.fromHandle h >> (lift $ hClose h)

pprint :: ServerEvent -> IO ()
pprint (UnknownServerEvent bs) = (pPrint . ("UnknownServerEvent: " <> ) . decodeUtf8) bs
pprint evt = pPrint evt

printLog file =
  runEffect
    (PP.mapM_ (pprint) <-<
     PA.parsed serverInputParser (loadServerEvents file))

findMobAlias substr = L.filter (T.isInfixOf substr . T.toLower . fst . snd) . zip [0..] . itoList  <$> loadCachedMobAliases
mobAlias i = view (ix i) . itoList  <$> loadCachedMobAliases

findMobData subst =
  T.intercalate "\n" .
  fmap renderMob .
  chunksOf 4 .
  toListOf
    (traversed .
     filtered (anyOf traversed (T.isInfixOf subst) . toListOf (mobFields)) .
     renderFields) .
  zip @Int [0 ..] . toList <$>
  loadCachedMobData >>= putStrLn
  where
    renderFields = (_1 . to showt) <> mobFields
    mobFields =
      _2 .
      nameCases .
      mconcat
        [ inRoomDesc . to unObjRef
        , nominative . to unObjRef
        , alias . to unObjRef
        ]

findLocation subst =
  T.intercalate "\n" .
  fmap renderLocation .
  M.elems . M.filter (T.isInfixOf subst . _locationTitle . _zloc) <$>
  loadCachedLocations >>=
  putStrLn

renderMob [i, r, n, a] = i <> "\t" <> r <> "\n\t" <> n <> "\n\t" <> a

renderLocation :: ZonedLocation -> Text
renderLocation loc = showt id <> "\t" <> z <> "\t" <> title
  where id = loc ^. zloc . locationId
        title = loc ^. zloc . locationTitle
        z = loc ^. zone

mobData i = toListOf (ix i) . toList  <$> loadCachedMobData


travelActions :: Monad m => Map (Int, Int) (Pipe Event Event m ())
travelActions = M.fromList [ ((6201, 6202), ddo "откр дверца" *> ddo "восток")
                           , ((5028, 5027), ddo "откр ворота" *> ddo "юг")
                           , ((5027, 5028), ddo "откр ворота" *> ddo "север")
                           , ((5102, 5107), ddo "откр дверь" *> ddo "север")
                           , ((6219, 6223), ddo "отпер дверь" *> ddo "откр дверь" *> ddo "юг")
                           , ((6053, 5054), ddo "держ свеч" *> ddo "дать 14 кун след" *> ddo "сн свеч")
                           , ((5054, 6053), ddo "держ свеч" *> ddo "дать 14 кун след" *> ddo "сн свеч")
                           ]

ddo :: Monad m => Text -> Pipe Event Event m ()
ddo = Pipes.yield . SendToServer
