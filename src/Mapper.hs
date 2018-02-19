{-# LANGUAGE OverloadedStrings #-}

module Mapper ( foldToDirections
              , foldToLocations
              , foldToItems
              , locsByRegex
              , showLocs
              , World(..)
              , Direction(..)
              ) where

import Data.ByteString hiding (head, empty, putStrLn)
import ServerInputParser
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph as DG
import System.IO hiding (putStrLn)
import Pipes
import Pipes.Prelude hiding (head, fromHandle)
import qualified Pipes.Prelude as PP
import Pipes.Attoparsec
import Pipes.ByteString hiding (head)
import qualified Pipes.ByteString as BS
import Data.Text hiding (head, empty)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Either
import Data.Maybe
import Control.Applicative
import Control.Arrow
import Data.Monoid
import Debug.Trace
import Prelude
import qualified Prelude as P
import Data.Set
import qualified Data.Set as S
import Event

data World = World { locations :: Set Location
                   , directions :: Set Direction
                   , items :: Set Item
                   }
type WorldMap = Gr Text Int
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
                                                                                         >-> PP.map (\(LocationEvent loc) -> loc))

foldToItems :: Monad m => Set Item -> Producer (Maybe (Either ParsingError ServerEvent)) m ()  -> m (Set Item)
foldToItems prevItems eventProducer = PP.fold foldToSet prevItems id (eventProducer >-> PP.filter filterItems
                                                                                   >-> PP.map unwrapJustRight
                                                                                   >-> PP.map (\(ItemStatsEvent item) -> item))

evtToLocation :: ServerEvent -> Location
evtToLocation (LocationEvent loc) = loc

unwrapJustRight :: Maybe (Either ParsingError ServerEvent) -> ServerEvent
unwrapJustRight (Just (Right evt)) = evt

filterLocationsAndMoves :: Maybe (Either ParsingError ServerEvent) -> Bool
filterLocationsAndMoves (Just (Right (LocationEvent _))) = True
filterLocationsAndMoves (Just (Right (MoveEvent _ ))) = True
filterLocationsAndMoves _ = False

filterLocations :: Maybe (Either ParsingError ServerEvent) -> Bool
filterLocations (Just (Right (LocationEvent _))) = True
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
             in case pair of ((LocationEvent locTo):(MoveEvent dir):(LocationEvent locFrom):[]) -> updateWorld locFrom locTo dir
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
mappableMove ((LocationEvent _) : (MoveEvent _) : (LocationEvent _) : []) = True
mappableMove _ = False

showLocs :: Set Location -> ByteString
showLocs locs = encodeUtf8 $ renderMsg locs
  where renderMsg = addRet . joinToOneMsg . S.toList . renderLocs
        joinToOneMsg = T.intercalate "\n"
        renderLocs = S.map renderLoc
        renderLoc node = (T.pack $ P.show $ locId node) <> " " <> locTitle node
        addRet txt = T.snoc txt '\n'

locsByRegex :: World -> Text -> Set Location
locsByRegex world regex = S.filter (T.isInfixOf regex . locTitle) locs
  where locs = locations world
