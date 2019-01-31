{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module World ( foldToDirections
             , locsByRegex
             , showLocs
             , loadWorld
             , parseProducer
             , showWorldStats
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

data World = World { worldMap :: WorldMap
                   , locations :: Set Location
                   , directions :: Set Direction
                   , items :: Set Item
                   , mobs :: Set MobRoomDesc
                   , questActions :: Map (LocationId, LocationId) [Event]
                   }
data Direction = Direction { locIdFrom :: LocIdFrom
                           , locIdTo :: LocIdTo
                           , trigger :: Trigger
                           } deriving (Eq, Show, Ord)
type LocIdFrom = LocationId
type LocIdTo = LocationId
type Trigger = Text
type WorldMap = Gr () Int

foldToDirections :: Monad m => Set Direction -> Producer (Maybe (Either ParsingError ServerEvent)) m ()  -> m (Set Direction)
foldToDirections initialDirections eventProducer = PP.fold accDirections initialDirections identity (eventProducer >-> PP.filter filterLocationsAndMoves
                                                                                                             >-> PP.map unwrapJustRight
                                                                                                             >-> PP.scan toPairs [] identity
                                                                                                             >-> PP.filter mappableMove
                                                                                              )

foldEntities :: (Monad m, Ord a) => Pipe ServerEvent [a] m () -> Producer (Maybe (Either ParsingError ServerEvent)) m ()  -> m (Set a)
foldEntities extractEntities eventProducer = PP.fold foldListToSet S.empty identity (eventProducer >-> PP.filter isJust
                                                                                   >-> PP.map fromJust
                                                                                   >-> PP.filter isRight
                                                                                   >-> PP.map unwrapRight
                                                                                   >-> extractEntities )

unwrapJustRight :: Maybe (Either ParsingError ServerEvent) -> ServerEvent
unwrapJustRight (Just (Right evt)) = evt

unwrapRight :: Either ParsingError ServerEvent -> ServerEvent
unwrapRight (Right val) = val

filterLocationsAndMoves :: Maybe (Either ParsingError ServerEvent) -> Bool
filterLocationsAndMoves (Just (Right LocationEvent{})) = True
filterLocationsAndMoves (Just (Right (MoveEvent _ ))) = True
filterLocationsAndMoves _ = False

filterLocations :: Maybe (Either ParsingError ServerEvent) -> Bool
filterLocations (Just (Right LocationEvent{})) = True
filterLocations _ = False

filterItems :: Maybe (Either ParsingError ServerEvent) -> Bool
filterItems (Just (Right (ItemStatsEvent item))) = True
filterItems _ = False

type SEPair = (Maybe ServerEvent, Maybe ServerEvent)

edgeWeight :: Int
edgeWeight = 1

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

foldToSet :: Ord a => Set a -> a -> Set a
foldToSet acc item = S.insert item acc

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
  where locs = locations world


loadDirections :: IO (Set Direction) -> FilePath -> IO (Set Direction)
loadDirections ioDirs file = do
  hLog <- openFile file ReadMode
  dirs <- ioDirs
  newDirs <- foldToDirections dirs $ parseProducer (PBS.fromHandle hLog)
  hClose hLog
  return newDirs

loadEntitiesFromFile :: Ord a => Pipe ServerEvent [a] IO () -> FilePath -> IO (Set a)
loadEntitiesFromFile extractEntities file = do
  hLog <- openFile file ReadMode
  result <- foldEntities extractEntities $ parseProducer (PBS.fromHandle hLog)
  hClose hLog
  return result

extractMobs :: Monad m => Pipe ServerEvent [MobRoomDesc] m ()
extractMobs = PP.filter isLocationEvent >-> PP.map (\(LocationEvent _ _ mobs) -> mobs)

extractLocs :: Monad m => Pipe ServerEvent [Location] m ()
extractLocs = PP.filter isLocationEvent >-> PP.map (\(LocationEvent loc _ _) -> [loc])

extractItemStats :: Monad m => Pipe ServerEvent [Item] m ()
extractItemStats = PP.map $ \case (ItemStatsEvent item) -> [item]
                                  _ -> []

loadQuestActions :: IO (Map (LocationId, LocationId) [Event]) -> FilePath -> IO (Map (LocationId, LocationId) [Event])
loadQuestActions accIO file = withLog file $ obstacleActions . filterTravelActions

loadWorld :: FilePath -> IO World
loadWorld archiveDir = do
  serverLogFiles <- listDirectory serverLogDir
  evtLogFiles <- listDirectory evtLogDir
  directions <- loadDirs serverLogFiles
  locations <- loadFromFiles extractLocs serverLogFiles
  itemsStats <- loadFromFiles extractItemStats serverLogFiles
  questActions <- loadQuestActs evtLogFiles
  mobs <- loadFromFiles extractMobs serverLogFiles
  let worldMap = buildMap directions
  return $ World worldMap locations directions itemsStats mobs questActions
    where loadDirs files = F.foldl (\acc item -> loadDirections acc (serverLogDir ++ item)) (return S.empty) files
          loadQuestActs files = F.foldl (\acc item -> loadQuestActions acc (evtLogDir ++ item)) (return M.empty) files
          loadFromFiles extractEntities files = F.foldl (\acc file -> S.union <$> acc <*> loadEntitiesFromFile extractEntities (serverLogDir ++ file)) (return S.empty) files
          serverLogDir = archiveDir ++ "server-input-log/"
          evtLogDir = archiveDir ++ "evt-log/"

showWorldStats :: World -> Producer Event IO ()
showWorldStats world = yield $ ConsoleOutput worldStats
  where worldStats = encodeUtf8 $ locationsStats <> mobsStats
        locationsStats = (show . length . locations) world <> " locations loaded\n"
        mobsStats = (show . length . mobs) world <> " mobs loaded\n"

parseProducer :: Producer ByteString IO () -> Producer (Maybe (Either ParsingError ServerEvent)) IO ()
parseProducer src = do
    (result, partial) <- liftIO $ runStateT (PA.parse serverInputParser) src
    continue result partial
      where continue result@(Just (Right _)) partial = do yield result
                                                          parseProducer partial
            continue (Just (Left (ParsingError ctxts err))) _ = liftIO $ C8.putStr $ "error: " <> C8.pack err <> C8.pack (L.concat ctxts) <> "\n"
            continue Nothing _ = liftIO $ C8.putStr "parsed entire stream\n"

buildMap :: Set Direction -> Gr () Int
buildMap directions = mkGraph nodes edges
  where edges = F.concat $ (\d -> [aheadEdge d, reverseEdge d]) <$> S.toList directions
        nodes = (\n -> (n, ())) <$> F.foldl (\acc (Direction (LocationId fromId) (LocationId toId) _) -> fromId : toId : acc) [] directions
        aheadEdge (Direction (LocationId fromId) (LocationId toId) _) = (fromId, toId, 1)
        reverseEdge (Direction (LocationId fromId) (LocationId toId) _) = (toId, fromId, 1)
