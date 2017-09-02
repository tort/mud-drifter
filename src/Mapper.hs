{-# LANGUAGE OverloadedStrings #-}

module Mapper (
  foldToGraph
  , locsByRegex
) where

import Data.ByteString hiding (head, empty, putStrLn)
import Parser
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

foldToGraph :: Monad m => Producer (Maybe (Either ParsingError ServerEvent)) m ()  -> m (Gr Text Text)
foldToGraph eventProducer = PP.fold accGraph DG.empty id (eventProducer >-> PP.filter filterLocationsAndMoves
                                                                  >-> PP.map unwrapLocationsAndMoves
                                                                  >-> PP.scan toPairs (Nothing, Nothing) id
                                                                  >-> PP.filter mappableMove
                                                                  >-> PP.map ((fromJust . fst) Control.Arrow.&&& (fromJust . snd)))


unwrapLocationsAndMoves :: Maybe (Either ParsingError ServerEvent) -> ServerEvent
unwrapLocationsAndMoves (Just (Right evt)) = evt

filterLocationsAndMoves :: Maybe (Either ParsingError ServerEvent) -> Bool
filterLocationsAndMoves (Just (Right (Location _))) = True
filterLocationsAndMoves (Just (Right (Move _ _))) = True
filterLocationsAndMoves _ = False

type SEPair = (Maybe ServerEvent, Maybe ServerEvent)

insNodeIfNotExist :: (Int, Text) -> Gr Text Text -> Gr Text Text
insNodeIfNotExist (id, name) graph
  | isJust $ lab graph id = graph
  | isNothing $ lab graph id = insNode (id, name) graph

insEdgeIfNotExist :: (Int, Int, Text) -> Gr Text Text -> Gr Text Text
insEdgeIfNotExist (from, to, dir) graph
  | hasEdge graph (from, to) = graph
  | otherwise = insEdge (from, to, dir) graph

accGraph :: Gr Text Text -> (ServerEvent, ServerEvent) -> Gr Text Text
accGraph graph (Location locFrom@(LocData fromId fromName), Move dir locTo@(LocData toId toName))
  | locFrom == locTo = graph
  | otherwise = (addMove . addTo . addFrom) graph
                where addFrom = insNodeIfNotExist (fromId, fromName)
                      addTo = insNodeIfNotExist (toId, toName)
                      addMove = insEdgeIfNotExist (fromId, toId, dir)
accGraph graph (Move _ locFrom@(LocData fromId fromName), Move dir locTo@(LocData toId toName))
  | locFrom == locTo = graph
  | otherwise = (addMove . addTo . addFrom) graph
                where addFrom = insNodeIfNotExist (fromId, fromName)
                      addTo = insNodeIfNotExist (toId, toName)
                      addMove = insEdgeIfNotExist (fromId, toId, dir)

toPairs :: (SEPair -> ServerEvent -> SEPair)
toPairs acc event = (snd acc, Just event)

mappableMove :: SEPair -> Bool
mappableMove (Just (Location _), Just (Move _ _)) = True
mappableMove (Just (Move _ _), Just (Move _ _)) = True
mappableMove _ = False

locsByRegex :: Gr Text Text -> Text -> ByteString
locsByRegex graph regex = encodeUtf8 $ renderMsg $ filterLocs locs
  where locs = labNodes graph
        renderMsg = addRet . joinToOneMsg . renderLocs
        joinToOneMsg = T.intercalate "\n"
        renderLocs = fmap renderLoc
        renderLoc node = (T.pack . P.show $ fst node) <> " " <> snd node
        addRet txt = T.snoc txt '\n'
        filterLocs = P.filter (T.isInfixOf regex . snd)
