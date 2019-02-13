{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module World ( locsByRegex
             , showLocs
             , loadWorld
             , parseProducer
             , printWorldStats
             , World(..)
             , Direction(..)
             , WorldMap
             ) where

import Protolude hiding ((<>), Location, runStateT, Down)
import qualified Data.ByteString.Char8 as C8
import ServerInputParser
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph as DG
import System.IO (hClose)
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as PP
import Pipes.Attoparsec
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PBS
import Data.Text()
import qualified Data.Text as T
import Data.Text.Encoding()
import Data.Either
import Data.Maybe
import Control.Applicative()
import Control.Arrow
import Data.Monoid
import Debug.Trace
import Event hiding (mobs)
import Data.Map.Strict hiding (insert)
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import Logger
import System.Directory
import Control.Lens

data World = World { _worldMap :: WorldMap
                   , _locations :: [Location]
                   , _directions :: [Direction]
                   , _itemsDiscovered :: Map ObjectRoomDesc (Map LocationId Int)
                   , _itemStats :: [ItemStats]
                   , _mobsDiscovered :: Map MobRoomDesc (Map LocationId Int)
                   , _mobStats :: [Mob]
                   , _questActions :: Map (LocationId, LocationId) [Event]
                   }
data Direction = Direction { locIdFrom :: LocIdFrom
                           , locIdTo :: LocIdTo
                           , trigger :: Trigger
                           } deriving (Eq, Show, Ord)
type LocIdFrom = LocationId
type LocIdTo = LocationId
type Trigger = Text
type WorldMap = Gr () Int

unwrapRight :: Either ParsingError ServerEvent -> ServerEvent
unwrapRight (Right val) = val

accDirections :: [Direction] -> [ServerEvent] -> [Direction]
accDirections directions pair =
  let updateWorld locFrom locTo dir
        | locFrom == locTo = directions
        | otherwise = insertOpposite $ insertAhead directions
        where insertAhead = cons (Direction (locFrom^.locationId) (locTo^.locationId) dir)
              insertOpposite = cons (Direction (locTo^.locationId) (locFrom^.locationId) $ oppositeDir dir)
   in case pair of [LocationEvent locTo _ _, MoveEvent dir, LocationEvent locFrom _ _] -> updateWorld locFrom locTo dir
                   _ -> directions

oppositeDir :: Text -> Text
oppositeDir "вверх" = "вниз"
oppositeDir "вниз" = "вверх"
oppositeDir "север" = "юг"
oppositeDir "юг" = "север"
oppositeDir "запад" = "восток"
oppositeDir "восток" = "запад"

toPairs :: [ServerEvent] -> ServerEvent -> [ServerEvent]
toPairs acc event
  | length acc < 3 = event : acc
  | otherwise = event : L.take 2 acc

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
locsByRegex world regex = L.filter (T.isInfixOf regex . T.toLower . (\l -> showVal $ l^.locationTitle)) locs
  where locs = _locations world

loadServerEventsFromFile :: FilePath -> Producer ByteString IO ()
loadServerEventsFromFile file = openfile >>= \h -> PBS.fromHandle h >> closefile h
  where openfile = lift $ openFile file ReadMode
        closefile h = lift $ hClose h

parseServerEvents :: Producer ByteString IO () -> Producer ServerEvent IO ()
parseServerEvents pbs = parseProducer pbs >-> PP.filter isServerEvent >-> PP.map _serverEvent

binEvtLogParser :: Producer ByteString IO () -> Producer Event IO ()
binEvtLogParser bsp = parseEventLogProducer =<< lift (PBS.toLazyM bsp) >-> PP.filter filterTravelActions

loadLogs :: [FilePath] -> Producer ByteString IO ()
loadLogs files = F.foldl (\evtPipe file -> evtPipe >> loadServerEventsFromFile file) (return ()) files

extractItems :: Monad m => Pipe ServerEvent [ObjectRoomDesc] m ()
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

extractLocs :: Monad m => Producer ServerEvent m () -> Producer Location m ()
extractLocs serverEvtProducer = serverEvtProducer >-> PP.filter isLocationEvent >-> PP.map (\(LocationEvent loc _ _) -> loc)

extractItemStats :: Monad m => Producer ServerEvent m () -> Producer ItemStats m ()
extractItemStats serverEvtProducer = serverEvtProducer >-> PP.filter isItemStatsEvent >-> PP.map (\(ItemStatsEvent item) -> item)

extractDirections :: Monad m => Producer ServerEvent m () -> m [Direction]
extractDirections producer = PP.fold accDirections [] identity (producer >-> PP.filter (\evt -> isLocationEvent evt || isMoveEvent evt)
                                                                                    >-> PP.scan toPairs [] identity
                                                                                    >-> PP.filter mappableMove)

listFilesIn :: FilePath -> IO [FilePath]
listFilesIn dir = ((dir ++ ) <$>) <$> listDirectory dir

loadWorld :: FilePath -> IO World
loadWorld currentDir = do
  serverLogFiles <- listFilesIn serverLogDir
  evtLogFiles <- listFilesIn evtLogDir
  directions <- (extractDirections . parseServerEvents . loadLogs) serverLogFiles
  locations <- PP.toListM $ (extractLocs . parseServerEvents . loadLogs) serverLogFiles
  itemsStats <- PP.toListM $ (extractItemStats . parseServerEvents . loadLogs) serverLogFiles
  items <- ((extractDiscovered _objects) . parseServerEvents . loadLogs) serverLogFiles
  mobs <- ((extractDiscovered _mobs) . parseServerEvents . loadLogs) serverLogFiles
  questActions <- (obstacleActions . binEvtLogParser . loadLogs) evtLogFiles
  let worldMap = buildMap directions
   in return World { _worldMap = worldMap
                     , _locations = locations
                     , _directions = directions
                     , _itemsDiscovered = items
                     , _itemStats = itemsStats
                     , _mobsDiscovered = mobs
                     , _mobStats = []
                     , _questActions = questActions
                     }
    where serverLogDir = archiveDir ++ "/server-input-log/"
          evtLogDir = archiveDir ++ "/evt-log/"
          archiveDir = currentDir ++ "/archive"

printWorldStats :: World -> Producer Event IO ()
printWorldStats world = yield $ ConsoleOutput worldStats
  where worldStats = encodeUtf8 $ locationsStats <> directionsStats <> items <> itemsStats <> mobs
        locationsStats = (show . length . _locations) world <> " локаций найдено\n"
        directionsStats = (show . length . _directions) world <> " переходов между локациями\n"
        items = (show . length . _itemsDiscovered) world <> " предметов найдено\n"
        itemsStats = (show . length . _itemStats) world <> " предметов опознано\n"
        mobs = (show . length . _mobsDiscovered) world <> " мобов найдено\n"

parseProducer :: Producer ByteString IO () -> Producer Event IO ()
parseProducer src = do (result, partial) <- liftIO $ runStateT (PA.parse serverInputParser) src
                       continue result partial
                         where continue result@(Just (Right evt)) partial = yield (ServerEvent evt) >> parseProducer partial
                               continue (Just (Left (ParsingError ctxts err))) _ = yield $ ConsoleOutput $ "error: " <> C8.pack err <> C8.pack (L.concat ctxts) <> "\n"
                               continue Nothing _ = yield $ ConsoleOutput "parsed entire stream\n"

buildMap :: [Direction] -> Gr () Int
buildMap directions = mkGraph nodes edges
  where edges = F.concat $ (\d -> [aheadEdge d, reverseEdge d]) <$> directions
        nodes = (\n -> (n, ())) <$> F.foldl (\acc (Direction (LocationId fromId) (LocationId toId) _) -> fromId : toId : acc) [] directions
        aheadEdge (Direction (LocationId fromId) (LocationId toId) _) = (fromId, toId, 1)
        reverseEdge (Direction (LocationId fromId) (LocationId toId) _) = (toId, fromId, 1)
