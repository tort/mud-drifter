{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Mapper ( printItems
              , printLocations
              , locationsBy
              , findLocationsBy
              , findTravelPath
              , showPath
              , zonePath
              , printMobsByRegex
              ) where

import Protolude hiding (Location, runStateT, intercalate)
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
import qualified Data.Graph.Inductive.Query.SP as SP
import qualified Data.Graph.Inductive.Query.MST as MST
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
showPath world (Just path) = render $ showDirection . lookupDir . toJust <$> nodePairs
  where render = encodeUtf8 . addRet . joinToOneMsg
        joinToOneMsg = T.intercalate ","
        showDirection (Just roomDir) = showt roomDir
        showDirection Nothing = "direction not found"
        addRet txt = T.snoc txt ','
        nodePairs = filterDirs $ scanl (\acc item -> (snd acc, Just item)) (Nothing, Nothing) path
        filterDirs = filter (\pair -> isJust (fst pair) && isJust (snd pair))
        toJust (Just left, Just right) = (left, right)
        lookupDir p = M.lookup p $ _directions world

findTravelPath :: LocationId -> LocationId -> WorldMap -> Maybe Path
findTravelPath (LocationId fromId) (LocationId toId) worldMap = (LocationId <$>) <$> (SP.sp fromId toId worldMap)

printItems :: Text -> Map ServerEvent (Map LocationId Int) -> Text
printItems subName itemsOnMap = (render . filterEvents) itemsOnMap
  where render mobs = M.foldMapWithKey renderMob mobs
        renderMob evt locToCountMap = renderEvent evt <> "\n" <> (renderLocs locToCountMap)
        renderLocs locToCountMap = mconcat $ showAssoc <$> (M.assocs locToCountMap)
        showAssoc (locId, count) = "\t" <> (showVal locId) <> ": " <> show count <> "\n"
        filterEvents = M.filterWithKey (\mobRoomDesc a -> filterEvent mobRoomDesc)
        filterEvent (ItemInTheRoom (ObjRef text)) = T.isInfixOf subName (T.toLower text)
        filterEvent (LootItem (ObjRef item) (ObjRef mob)) = T.isInfixOf subName (T.toLower item)
        filterEvent (TakeFromContainer (ObjRef item) (ObjRef container)) = T.isInfixOf subName (T.toLower item)
        filterEvent (MobGaveYouItem (ObjRef mob) (ObjRef item)) = T.isInfixOf subName (T.toLower item)
        renderEvent (ItemInTheRoom (ObjRef text)) = text
        renderEvent (LootItem (ObjRef item) (ObjRef mob)) = "Вы взяли " <> item <> " из трупа " <> mob
        renderEvent (TakeFromContainer (ObjRef item) (ObjRef container)) = "Вы взяли " <> item <> " из " <> container
        renderEvent (MobGaveYouItem (ObjRef mob) (ObjRef item)) = mob <> " дал вам " <> item

printLocations :: Text -> World -> IO ()
printLocations substr world = mapM_ printT $ locationsBy substr world

printMobsByRegex :: World -> Text -> IO ()
printMobsByRegex world regex = mapM_ printT $ L.filter (\(ObjRef t) -> T.isInfixOf regex $ T.toLower t) $ M.keys $ _mobsDiscovered world

findLocationsBy :: Text -> World -> [LocationId]
findLocationsBy substr world = _locationId <$> locationsBy substr world

locationsBy :: Text -> World -> [Location]
locationsBy substr world = filter (T.isInfixOf (T.toLower substr) . T.toLower . showt) (_locations world ^.. folded)

sharePrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
sharePrefix l1 l2 = let prefix = map fst $ takeWhile (uncurry (==)) $ zip l1 l2
                        f = drop $ length prefix
                     in (prefix, f l1, f l2)

listToPairs :: [a] -> [(a,a)]
listToPairs list = fmap unwrap . filterDirs . scanToPairs $ list
  where filterDirs = filter (\(l, r) -> isJust l && isJust r)
        unwrap (Just left, Just right) = (left, right)
        scanToPairs = scanl (\acc item -> (snd acc, Just item)) (Nothing, Nothing)

zonePath :: World -> Int -> [LocationId]
zonePath world anyZoneLocId = fmap LocationId $ concat $  mergeTree $ listToPairs (reverse . fmap fst . unLPath <$> MST.msTree zone)
  where zone = zoneMap world anyZoneLocId
        mergeTree = foldr (\(l, r) acc -> mergePathPair l r : acc) []
        mergePathPair l r = let (commonPath, lPrivatePath, rPrivatePath) = sharePrefix l r
                                lastCommonNode = maybeToList . head . reverse $ commonPath
                                privatePathBack [] = []
                                privatePathBack (xs) = L.tail $ reverse $ lastCommonNode ++ xs
                                in concat [ privatePathBack lPrivatePath, rPrivatePath ]
