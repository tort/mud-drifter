{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Mapper ( mapper
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
import Pipes.Safe
import Control.Lens hiding (snoc, (<>))
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.List as L

mapper :: MonadSafe m => World -> Pipe Event Event m ()
mapper world = let mapperWithPosition currLoc = do evt <- await
                                                   case evt of (ServerEvent (LocationEvent loc _ _)) -> yield evt >> mapperWithPosition (Just $ loc^.locationId)
                                                               _ -> do yield $ case evt of (UserCommand (FindLoc text)) -> ConsoleOutput $ showLocs $ locsByRegex world text
                                                                                           (UserCommand (FindPathFromTo from to)) -> ConsoleOutput $ showPathBy world (Just from) to
                                                                                           (UserCommand (FindPathToLocId to)) -> ConsoleOutput $ showPathBy world currLoc to
                                                                                           (UserCommand (WhereMob subName)) -> ConsoleOutput $ showAreal subName _mobsDiscovered world
                                                                                           (UserCommand (WhereObject subName)) -> ConsoleOutput $ showItemAreal subName (_itemsDiscovered world)
                                                                                           (UserCommand (FindPathTo regex)) -> let matchingLocs = locsByRegex world regex
                                                                                                                                in ConsoleOutput $ case matchingLocs of
                                                                                                                                  [] -> "no matching locations found"
                                                                                                                                  d:[] -> showPathBy world currLoc (d^.locationId)
                                                                                                                                  _ -> showLocs matchingLocs
                                                                                           x -> x
                                                                       mapperWithPosition currLoc
                in mapperWithPosition Nothing

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
nodePairToDirection world (from, to) = L.head $ filter (\(Direction dirFrom dirTo _) -> dirFrom == from && dirTo == to) (_directions world)

showPathBy :: World -> Maybe LocationId -> LocationId -> ByteString
showPathBy world Nothing _ = "current location is unknown\n"
showPathBy world (Just fromId) toId = if (fromId == toId) then "you are already there!"
                                                          else showPath world $ findTravelPath fromId toId (_worldMap world)

findTravelPath :: LocationId -> LocationId -> WorldMap -> Maybe [LocationId]
findTravelPath (LocationId fromId) (LocationId toId) worldMap = (LocationId <$>) <$> (GA.sp fromId toId worldMap)

showItemAreal :: Text -> Map ServerEvent (Map LocationId Int) -> ByteString
showItemAreal subName items = (renderr . limit . filterMobs) items
  where renderr mobs = encodeUtf8 $ M.foldMapWithKey renderMob mobs
        renderMob evt locToCountMap = renderEvent evt <> "\n" <> (renderLocs locToCountMap)
        renderLocs locToCountMap = mconcat $ showAssoc <$> (M.assocs locToCountMap)
        showAssoc (locId, count) = "\t" <> (showVal locId) <> ": " <> show count <> "\n"
        limit = M.take 5
        filterMobs = M.filterWithKey (\mobRoomDesc a -> filterMob mobRoomDesc)
        filterMob (ItemInTheRoom itemDesc) = T.isInfixOf subName (T.toLower $ showVal itemDesc)
        filterMob (LootCorpse itemDesc mob) = T.isInfixOf subName (T.toLower $ showVal itemDesc)
        renderEvent (ItemInTheRoom itemDesc) = "На земле: " <> showVal itemDesc
        renderEvent (LootCorpse itemDesc mob) = "В трупе " <> showVal mob <> ": " <> showVal itemDesc

showAreal :: ShowVal a => Text -> (World -> Map a (Map LocationId Int)) -> World -> ByteString
showAreal subName getter world = renderr . limit . filterMobs $ getter world
  where renderr mobs = encodeUtf8 $ M.foldMapWithKey renderMob mobs
        renderMob entity locToCountMap = showVal entity <> "\n" <> (renderLocs locToCountMap)
        renderLocs locToCountMap = mconcat $ showAssoc <$> (M.assocs locToCountMap)
        showAssoc (locId, count) = "\t" <> (showVal locId) <> ": " <> show count <> "\n"
        limit = M.take 5
        filterMobs = M.filterWithKey (\mobRoomDesc a -> filterMob mobRoomDesc)
        filterMob entity = T.isInfixOf subName (T.toLower $ showVal entity)

{-showFindPathResponse :: World -> Maybe Int -> Event -> ByteString
showFindPathResponse world currLoc userInput =
  let destination = locsByRegex world
   in case (currLoc, userInput) of
        (_, (UserCommand (FindPathFromTo from to))) -> showPathBy from to
        (Just currLoc, (UserCommand (FindPathToLocId to))) -> showPathBy currLoc to
        (Just currLoc, (UserCommand (FindPathTo regex))) -> let matchingLocs = destination regex
                                             in case S.toList $ matchingLocs of
                                                  [] -> "no matching locations found"
                                                  d:[] -> showPathBy currLoc (locId d)
                                                  _ -> showLocs matchingLocs
        (Nothing, _) -> "current location is unknown\n"
-}
