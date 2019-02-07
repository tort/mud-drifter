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
import Data.Text
import Event
import World
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph hiding (Path)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Graph.Inductive.Query.SP as GA
import Pipes.Safe
import Control.Lens hiding (snoc, (<>))
import Data.Foldable
import qualified Data.Map.Strict as M

mapper :: MonadSafe m => World -> Pipe Event Event m ()
mapper world = let mapperWithPosition currLoc = do evt <- await
                                                   case evt of (ServerEvent (LocationEvent loc _ _)) -> yield evt >> mapperWithPosition (Just $ loc^.locationId)
                                                               _ -> do yield $ case evt of (UserCommand (FindLoc text)) -> ConsoleOutput $ showLocs $ locsByRegex world text
                                                                                           (UserCommand (FindPathFromTo from to)) -> ConsoleOutput $ showPathBy world (Just from) to
                                                                                           (UserCommand (FindPathToLocId to)) -> ConsoleOutput $ showPathBy world currLoc to
                                                                                           (UserCommand (WhereMob subRoomName)) -> ConsoleOutput $ showMobAreal subRoomName world
                                                                                           (UserCommand (FindPathTo regex)) -> let matchingLocs = locsByRegex world regex
                                                                                                                                in ConsoleOutput $ case S.toList $ matchingLocs of
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
  where joinToOneMsg = intercalate "\n"
        showDirection = trigger
        addRet txt = snoc txt '\n'
        nodePairs = filterDirs $ L.scanl (\acc item -> (snd acc, Just item)) (Nothing, Nothing) path
        filterDirs = L.filter (\pair -> isJust (fst pair) && isJust (snd pair))
        toJust (Just left, Just right) = (left, right)

nodePairToDirection :: World -> (LocationId, LocationId) -> Direction
nodePairToDirection world (from, to) = L.head $ S.toList $ S.filter (\(Direction dirFrom dirTo _) -> dirFrom == from && dirTo == to) (_directions world)

showPathBy :: World -> Maybe LocationId -> LocationId -> ByteString
showPathBy world Nothing _ = "current location is unknown\n"
showPathBy world (Just fromId) toId = if (fromId == toId) then "you are already there!"
                                                          else showPath world $ findTravelPath fromId toId (_worldMap world)

findTravelPath :: LocationId -> LocationId -> WorldMap -> Maybe [LocationId]
findTravelPath (LocationId fromId) (LocationId toId) worldMap = (LocationId <$>) <$> (GA.sp fromId toId worldMap)

showMobAreal :: Text -> World -> ByteString
showMobAreal subName world = renderr . limit . filterMobs $ mobs
  where mobs = _mobsDiscovered world
        renderr mobs = encodeUtf8 $ M.foldMapWithKey renderMob mobs
        renderMob (MobRoomDesc desc) locToCountMap = desc <> "\n" <> (renderLocs locToCountMap)
        renderLocs locToCountMap = mconcat $ showAssoc <$> (M.assocs locToCountMap) :: Text
        showAssoc (locId, count) = "\t" <> (showVal locId) <> ": " <> show count <> "\n"
        limit = M.take 5
        filterMobs = M.filterWithKey (\mobRoomDesc a -> filterMob mobRoomDesc)
        filterMob (MobRoomDesc desc) = isInfixOf subName (toLower desc)

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
