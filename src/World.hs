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
import qualified Data.List as L
import Control.Applicative()
import Control.Arrow
import Data.Monoid
import Debug.Trace
import Data.Set
import qualified Data.Set as S
import Event hiding (mobs)
import Data.Map.Strict hiding (insert)
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import Logger
import System.Directory
import Control.Lens

data World = World { _worldMap :: WorldMap
                   , _locations :: Set Location
                   , _directions :: Set Direction
                   , _itemsDiscovered :: Set ObjectRoomDesc
                   , _itemStats :: Set ItemStats
                   , _mobsDiscovered :: Set MobRoomDesc
                   , _mobStats :: Set Mob
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

accDirections :: Set Direction -> [ServerEvent] -> Set Direction
accDirections directions pair =
  let updateWorld locFrom locTo dir
        | locFrom == locTo = directions
        | otherwise = insertOpposite $ insertAhead directions
        where insertAhead = insert (Direction (locFrom^.locationId) (locTo^.locationId) dir)
              insertOpposite = insert (Direction (locTo^.locationId) (locFrom^.locationId) $ oppositeDir dir)
   in case pair of [LocationEvent locTo _ _, MoveEvent dir, LocationEvent locFrom _ _] -> updateWorld locFrom locTo dir
                   _ -> directions

oppositeDir :: Text -> Text
oppositeDir "вверх" = "вниз"
oppositeDir "вниз" = "вверх"
oppositeDir "север" = "юг"
oppositeDir "юг" = "север"
oppositeDir "запад" = "восток"
oppositeDir "восток" = "запад"

foldListToSet :: Ord a => Set a -> [a] -> Set a
foldListToSet = F.foldl (flip S.insert)

toPairs :: [ServerEvent] -> ServerEvent -> [ServerEvent]
toPairs acc event
  | length acc < 3 = event : acc
  | otherwise = event : take 2 acc

mappableMove :: [ServerEvent] -> Bool
mappableMove [LocationEvent{}, MoveEvent _, LocationEvent{}] = True
mappableMove _ = False

showLocs :: Set Location -> ByteString
showLocs locs = encodeUtf8 $ renderMsg locs
  where renderMsg = addRet . joinToOneMsg . S.toList . renderLocs
        joinToOneMsg = T.intercalate "\n"
        renderLocs = S.map renderLoc
        renderLoc node = (showVal (node^.locationId) <> " " <> showVal (node^.locationTitle)) :: Text
        addRet txt = T.snoc txt '\n'

locsByRegex :: World -> Text -> Set Location
locsByRegex world regex = S.filter (T.isInfixOf regex . T.toLower . (\l -> showVal $ l^.locationTitle)) locs
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

extractLocs :: Monad m => Pipe ServerEvent [Location] m ()
extractLocs = PP.filter isLocationEvent >-> PP.map (\(LocationEvent loc _ _) -> [loc])

extractItemStats :: Monad m => Pipe ServerEvent [ItemStats] m ()
extractItemStats = PP.map $ \case (ItemStatsEvent item) -> [item]
                                  _ -> []

extractEntitiesSet :: (Monad m, Ord a) => Pipe ServerEvent [a] m () -> Producer ServerEvent m () -> m (Set a)
extractEntitiesSet extractEntities producer = PP.fold foldListToSet S.empty identity (producer >-> extractEntities)

extractDirections :: Monad m => Producer ServerEvent m () -> m (Set Direction)
extractDirections producer = PP.fold accDirections S.empty identity (producer >-> PP.filter (\evt -> isLocationEvent evt || isMoveEvent evt)
                                                                                    >-> PP.scan toPairs [] identity
                                                                                    >-> PP.filter mappableMove)

listFilesIn :: FilePath -> IO [FilePath]
listFilesIn dir = ((dir ++ ) <$>) <$> listDirectory dir

loadWorld :: FilePath -> IO World
loadWorld archiveDir = do
  serverLogFiles <- listFilesIn serverLogDir
  evtLogFiles <- listFilesIn evtLogDir
  directions <- (extractDirections . parseServerEvents . loadLogs) serverLogFiles
  locations <- (extractEntitiesSet extractLocs . parseServerEvents . loadLogs) serverLogFiles
  itemsStats <- (extractEntitiesSet extractItemStats . parseServerEvents . loadLogs) serverLogFiles
  items <- (extractEntitiesSet extractItems . parseServerEvents . loadLogs) serverLogFiles
  mobs <- (extractEntitiesSet extractMobs . parseServerEvents . loadLogs) serverLogFiles
  questActions <- (obstacleActions . binEvtLogParser . loadLogs) evtLogFiles
  let worldMap = buildMap directions
   in return World { _worldMap = worldMap
                     , _locations = locations
                     , _directions = directions
                     , _itemsDiscovered = items
                     , _itemStats = itemsStats
                     , _mobsDiscovered = mobs
                     , _mobStats = S.empty
                     , _questActions = questActions
                     }
    where serverLogDir = archiveDir ++ "server-input-log/"
          evtLogDir = archiveDir ++ "evt-log/"

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

buildMap :: Set Direction -> Gr () Int
buildMap directions = mkGraph nodes edges
  where edges = F.concat $ (\d -> [aheadEdge d, reverseEdge d]) <$> S.toList directions
        nodes = (\n -> (n, ())) <$> F.foldl (\acc (Direction (LocationId fromId) (LocationId toId) _) -> fromId : toId : acc) [] directions
        aheadEdge (Direction (LocationId fromId) (LocationId toId) _) = (fromId, toId, 1)
        reverseEdge (Direction (LocationId fromId) (LocationId toId) _) = (toId, fromId, 1)
