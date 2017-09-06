{-# LANGUAGE OverloadedStrings #-}

module Mapper ( foldToDirections
              , foldToLocations
              , locsByRegex
              , World(..)
              , Direction(..)
              , Location(..)
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

data World = World { locations :: Set Location
                   , directions :: Set Direction
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
                                                                  >-> PP.map unwrapLocationsAndMoves
                                                                  >-> PP.scan toPairs (Nothing, Nothing) id
                                                                  >-> PP.filter mappableMove
                                                                  >-> PP.map ((fromJust . fst) Control.Arrow.&&& (fromJust . snd)))

foldToLocations :: Monad m => Set Location -> Producer (Maybe (Either ParsingError ServerEvent)) m ()  -> m (Set Location)
foldToLocations prevLocs eventProducer = PP.fold accLocations prevLocs id (eventProducer >-> PP.filter filterLocationsAndMoves
                                                                                         >-> PP.map unwrapLocationsAndMoves
                                                                                         >-> PP.map evtToLocation)

evtToLocation :: ServerEvent -> Location
evtToLocation (LocationEvent loc) = loc
evtToLocation (MoveEvent dir loc) = loc

unwrapLocationsAndMoves :: Maybe (Either ParsingError ServerEvent) -> ServerEvent
unwrapLocationsAndMoves (Just (Right evt)) = evt

filterLocationsAndMoves :: Maybe (Either ParsingError ServerEvent) -> Bool
filterLocationsAndMoves (Just (Right (LocationEvent _))) = True
filterLocationsAndMoves (Just (Right (MoveEvent _ _))) = True
filterLocationsAndMoves _ = False

type SEPair = (Maybe ServerEvent, Maybe ServerEvent)

edgeWeight :: Int
edgeWeight = 1

accDirections :: Set Direction -> (ServerEvent, ServerEvent) -> Set Direction
accDirections directions pair =
  let updateWorld locFrom locTo dir
        | locFrom == locTo = directions
        | otherwise = insertOpposite $ insertAhead directions
          where insertAhead = insert (Direction (locId locFrom) (locId locTo) dir)
                insertOpposite = insert (Direction (locId locTo) (locId locFrom) $ oppositeDir dir)
   in case pair of (LocationEvent locFrom, MoveEvent dir locTo) -> updateWorld locFrom locTo dir
                   (MoveEvent _ locFrom, MoveEvent dir locTo) -> updateWorld locFrom locTo dir

oppositeDir :: Text -> Text
oppositeDir "вверх" = "вниз"
oppositeDir "вниз" = "вверх"
oppositeDir "север" = "юг"
oppositeDir "юг" = "север"
oppositeDir "запад" = "восток"
oppositeDir "восток" = "запад"

accLocations :: Set Location -> Location -> Set Location
accLocations locations newLocation = S.insert newLocation locations

toPairs :: (SEPair -> ServerEvent -> SEPair)
toPairs acc event = (snd acc, Just event)

mappableMove :: SEPair -> Bool
mappableMove (Just (LocationEvent _), Just (MoveEvent _ _)) = True
mappableMove (Just (MoveEvent _ _), Just (MoveEvent _ _)) = True
mappableMove _ = False

locsByRegex :: World -> Text -> ByteString
locsByRegex world regex = encodeUtf8 $ renderMsg $ filterLocs locs
  where locs = locations world
        renderMsg = addRet . joinToOneMsg . S.toList . renderLocs
        joinToOneMsg = T.intercalate "\n"
        renderLocs = S.map renderLoc
        renderLoc node = (T.pack $ P.show $ locId node) <> " " <> locTitle node
        addRet txt = T.snoc txt '\n'
        filterLocs = S.filter (T.isInfixOf regex . locTitle)
