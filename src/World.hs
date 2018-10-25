{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module World ( foldToDirections
             , foldToLocations
             , foldToItems
             , locsByRegex
             , showLocs
             , loadWorld
             , parseProducer
             , World(..)
             , Direction(..)
             ) where

import Protolude hiding ((<>), Location, runStateT)
import qualified Data.ByteString.Char8 as C8
import ServerInputParser
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph as DG
import System.IO hiding (putStrLn)
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as PP
import Pipes.Attoparsec
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PBS
import Data.Text()
import qualified Data.Text as T
import Data.Text.Encoding()
import Data.Either()
import Data.Maybe()
import qualified Data.List as L
import Control.Applicative()
import Control.Arrow
import Data.Monoid
import Debug.Trace
import Prelude
import qualified Prelude as P
import Data.Set
import qualified Data.Set as S
import Event
import Data.Map.Strict hiding (insert)
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import Logger
import System.Directory

data World = World { worldMap :: Gr () Int
                   , locations :: Set Location
                   , directions :: Set Direction
                   , items :: Set Item
                   , questActions :: Map (LocId, LocId) [Event]
                   }
data Direction = Direction { locIdFrom :: LocIdFrom
  , locIdTo :: LocIdTo
  , trigger :: Trigger
                           } deriving (Eq, Show, Ord)
type LocIdFrom = LocId
type LocIdTo = LocId
type Trigger = Text

foldToDirections :: Monad m => Set Direction -> Producer (Maybe (Either ParsingError ServerEvent)) m ()  -> m (Set Direction)
foldToDirections initialDirections eventProducer = PP.fold accDirections initialDirections id (eventProducer >-> PP.filter filterLocationsAndMoves
                                                                                                             >-> PP.map unwrapJustRight
                                                                                                             >-> PP.scan toPairs [] id
                                                                                                             >-> PP.filter mappableMove
                                                                                              )

foldToLocations :: Monad m => Set Location -> Producer (Maybe (Either ParsingError ServerEvent)) m ()  -> m (Set Location)
foldToLocations prevLocs eventProducer = PP.fold foldToSet prevLocs id (eventProducer >-> PP.filter filterLocations
                                                                                         >-> PP.map unwrapJustRight
                                                                                         >-> PP.map (\(LocationEvent loc _) -> loc))

foldToItems :: Monad m => Set Item -> Producer (Maybe (Either ParsingError ServerEvent)) m ()  -> m (Set Item)
foldToItems prevItems eventProducer = PP.fold foldToSet prevItems id (eventProducer >-> PP.filter filterItems
                                                                                   >-> PP.map unwrapJustRight
                                                                                   >-> PP.map (\(ItemStatsEvent item) -> item))

unwrapJustRight :: Maybe (Either ParsingError ServerEvent) -> ServerEvent
unwrapJustRight (Just (Right evt)) = evt

filterLocationsAndMoves :: Maybe (Either ParsingError ServerEvent) -> Bool
filterLocationsAndMoves (Just (Right (LocationEvent _ _))) = True
filterLocationsAndMoves (Just (Right (MoveEvent _ ))) = True
filterLocationsAndMoves _ = False

filterLocations :: Maybe (Either ParsingError ServerEvent) -> Bool
filterLocations (Just (Right (LocationEvent _ _))) = True
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
        where insertAhead = insert (Direction (locId locFrom) (locId locTo) dir)
              insertOpposite = insert (Direction (locId locTo) (locId locFrom) $ oppositeDir dir)
   in case pair of ((LocationEvent locTo _):(MoveEvent dir):(LocationEvent locFrom _):[]) -> updateWorld locFrom locTo dir
                   _ -> directions

oppositeDir :: Text -> Text
oppositeDir "вверх" = "вниз"
oppositeDir "вниз" = "вверх"
oppositeDir "север" = "юг"
oppositeDir "юг" = "север"
oppositeDir "запад" = "восток"
oppositeDir "восток" = "запад"

foldToSet :: Ord a => Set a -> a -> Set a
foldToSet acc item = S.insert item acc

toPairs :: [ServerEvent] -> ServerEvent -> [ServerEvent]
toPairs acc event
  | P.length acc < 3 = event : acc
  | otherwise = event : P.take 2 acc

mappableMove :: [ServerEvent] -> Bool
mappableMove ((LocationEvent _ _) : (MoveEvent _) : (LocationEvent _ _) : []) = True
mappableMove _ = False

showLocs :: Set Location -> ByteString
showLocs locs = encodeUtf8 $ renderMsg locs
  where renderMsg = addRet . joinToOneMsg . S.toList . renderLocs
        joinToOneMsg = T.intercalate "\n"
        renderLocs = S.map renderLoc
        renderLoc node = (T.pack $ P.show $ locId node) <> " " <> locTitle node
        addRet txt = T.snoc txt '\n'

locsByRegex :: World -> Text -> Set Location
locsByRegex world regex = S.filter (T.isInfixOf regex . T.toLower . locTitle) locs
  where locs = locations world


loadDirections :: IO (Set Direction) -> FilePath -> IO (Set Direction)
loadDirections ioDirs file = do
  hLog <- openFile file ReadMode
  dirs <- ioDirs
  newDirs <- foldToDirections dirs $ parseProducer (PBS.fromHandle hLog)
  hClose hLog
  return newDirs

loadLocations :: IO (Set Location) -> FilePath -> IO (Set Location)
loadLocations ioLocs file = do
  hLog <- openFile file ReadMode
  locs <- ioLocs
  newLocations <- foldToLocations locs $ parseProducer (PBS.fromHandle hLog)
  hClose hLog
  return newLocations

loadItems :: IO (Set Item) -> FilePath -> IO (Set Item)
loadItems accIO file = do
  hLog <- openFile file ReadMode
  items <- accIO
  result <- foldToItems items $ parseProducer (PBS.fromHandle hLog)
  hClose hLog
  return result

loadQuestActions :: IO (Map (Int, Int) [Event]) -> FilePath -> IO (Map (Int, Int) [Event])
loadQuestActions accIO file = withLog file $ obstacleActions . filterTravelActions

loadWorld :: FilePath -> IO World
loadWorld archiveDir = do
  serverLogFiles <- listDirectory serverLogDir
  evtLogFiles <- listDirectory evtLogDir
  directions <- loadDirs serverLogFiles
  locations <- loadLocs serverLogFiles
  items <- loadItms serverLogFiles
  questActions <- loadQuestActs evtLogFiles
  let worldMap = buildMap directions
  return $ World worldMap locations directions items questActions
    where loadDirs files = F.foldl (\acc item -> loadDirections acc (serverLogDir ++ item)) (return S.empty) files
          loadLocs files = F.foldl (\acc item -> loadLocations acc (serverLogDir ++ item)) (return S.empty) files
          loadItms files = F.foldl (\acc item -> loadItems acc (serverLogDir ++ item)) (return S.empty) files
          loadQuestActs files = F.foldl (\acc item -> loadQuestActions acc (evtLogDir ++ item)) (return M.empty) files
          serverLogDir = archiveDir ++ "server-input-log/"
          evtLogDir = archiveDir ++ "evt-log/"

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
  where edges = F.concat $ (\d -> aheadEdge d : reverseEdge d : []) <$> (S.toList directions)
        nodes = fmap (\n -> (n, ())) $ F.foldl (\acc d -> locIdFrom d : locIdTo d : acc) [] directions
        aheadEdge d = (locIdFrom d, locIdTo d, 1)
        reverseEdge d = (locIdTo d, locIdFrom d, 1)