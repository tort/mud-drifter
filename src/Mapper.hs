{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Mapper ( showItemAreal
              , printLocations
              , locationsBy
              , findLocationsBy
              , findTravelPath
              ) where

import Protolude hiding (Location, runStateT, head, intercalate)
import Data.ByteString.Char8()
import ServerInputParser
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as PP
import qualified Data.Text as T
import Event
import World
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph hiding (Path)
import qualified Data.Graph.Inductive.Query.SP as GA
import Control.Lens hiding (snoc, (<>))
import Data.Foldable
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import TextShow

type Path = [LocationId]

showPath :: World -> Maybe Path -> ByteString
showPath world Nothing = "where is no path there\n"
showPath world (Just []) = "path is empty\n"
showPath world (Just path) = (encodeUtf8 . addRet . joinToOneMsg) (showDirection . (nodePairToDirection world) <$> toJust <$> nodePairs)
  where joinToOneMsg = T.intercalate "\n"
        showDirection = trigger
        addRet txt = T.snoc txt '\n'
        nodePairs = filterDirs $ scanl (\acc item -> (snd acc, Just item)) (Nothing, Nothing) path
        filterDirs = filter (\pair -> isJust (fst pair) && isJust (snd pair))
        toJust (Just left, Just right) = (left, right)

nodePairToDirection :: World -> (LocationId, LocationId) -> Direction
nodePairToDirection world (from, to) = L.head $ S.toList $ S.filter (\(Direction dirFrom dirTo _) -> dirFrom == from && dirTo == to) (_directions world)

showPathBy :: World -> Maybe LocationId -> LocationId -> ByteString
showPathBy world Nothing _ = "current location is unknown\n"
showPathBy world (Just fromId) toId = if (fromId == toId) then "you are already there!"
                                                          else showPath world $ findTravelPath fromId toId (_worldMap world)

findTravelPath :: LocationId -> LocationId -> WorldMap -> Maybe [LocationId]
findTravelPath (LocationId fromId) (LocationId toId) worldMap = (LocationId <$>) <$> (GA.sp fromId toId worldMap)

showItemAreal :: Text -> World -> ByteString
showItemAreal subName world = (renderr . filterEvents) (_itemsOnMap world)
  where renderr mobs = encodeUtf8 $ M.foldMapWithKey renderMob mobs
        renderMob evt locToCountMap = renderEvent evt <> "\n" <> (renderLocs locToCountMap)
        renderLocs locToCountMap = mconcat $ showAssoc <$> (M.assocs locToCountMap)
        showAssoc (locId, count) = "\t" <> (showVal locId) <> ": " <> show count <> "\n"
        filterEvents = M.filterWithKey (\mobRoomDesc a -> filterEvent mobRoomDesc)
        filterEvent (ItemInTheRoom (ItemRoomDesc text)) = T.isInfixOf subName (T.toLower text)
        filterEvent (LootCorpse (ItemAccusative item) (MobGenitive mob)) = T.isInfixOf subName (T.toLower item)
        filterEvent (TakeFromContainer (ItemAccusative item) (ItemGenitive container)) = T.isInfixOf subName (T.toLower item)
        filterEvent (MobGaveYouItem (MobNominative mob) (ItemAccusative item)) = T.isInfixOf subName (T.toLower item)
        renderEvent (ItemInTheRoom (ItemRoomDesc text)) = text
        renderEvent (LootCorpse (ItemAccusative item) (MobGenitive mob)) = "Вы взяли " <> item <> " из трупа " <> mob
        renderEvent (TakeFromContainer (ItemAccusative item) (ItemGenitive container)) = "Вы взяли " <> item <> " из " <> container
        renderEvent (MobGaveYouItem (MobNominative mob) (ItemAccusative item)) = mob <> " дал вам " <> item

showAreal :: ShowVal a => Text -> (World -> Map a (Map LocationId Int)) -> World -> ByteString
showAreal subName getter world = renderr . limit . filterEvents $ getter world
  where renderr mobs = encodeUtf8 $ M.foldMapWithKey renderMob mobs
        renderMob entity locToCountMap = showVal entity <> "\n" <> (renderLocs locToCountMap)
        renderLocs locToCountMap = mconcat $ showAssoc <$> (M.assocs locToCountMap)
        showAssoc (locId, count) = "\t" <> (showVal locId) <> ": " <> show count <> "\n"
        limit = M.take 5
        filterEvents = M.filterWithKey (\mobRoomDesc a -> filterEvent mobRoomDesc)
        filterEvent entity = T.isInfixOf subName (T.toLower $ showVal entity)

printLocations :: Text -> World -> IO ()
printLocations substr world = mapM_ printT $ locationsBy substr world

findLocationsBy :: Text -> World -> [LocationId]
findLocationsBy substr world = _locationId <$> locationsBy substr world

locationsBy :: Text -> World -> [Location]
locationsBy substr world = filter (T.isInfixOf substr . T.toLower . showt) (_locations world ^.. folded)
