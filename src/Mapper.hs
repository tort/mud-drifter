{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Mapper (locationsBy
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
import TextShow.Generic

type Path = [Int]

showPath :: World -> Maybe Path -> ByteString
showPath world Nothing = "where is no path there\n"
showPath world (Just []) = "path is empty\n"
showPath world (Just path) = render $ showDirection . lookupDir . toJust <$> nodePairs
  where render = encodeUtf8 . addRet . joinToOneMsg
        joinToOneMsg = T.intercalate ","
        showDirection (Just roomDir) = genericShowt roomDir
        showDirection Nothing = "direction not found"
        addRet txt = T.snoc txt ','
        nodePairs = filterDirs $ scanl (\acc item -> (snd acc, Just item)) (Nothing, Nothing) path
        filterDirs = filter (\pair -> isJust (fst pair) && isJust (snd pair))
        toJust (Just left, Just right) = (left, right)
        lookupDir p = M.lookup p $ _directions world

findTravelPath :: Int -> Int -> WorldMap -> Maybe Path
findTravelPath (fromId) (toId) worldMap = (SP.sp fromId toId worldMap)

printMobsByRegex :: World -> Text -> IO ()
printMobsByRegex world regex = mapM_ genericPrintT $ L.filter (\(ObjRef t) -> T.isInfixOf regex $ T.toLower t) $ M.keys $ _inRoomDescToMobOnMap world

findLocationsBy :: Text -> World -> [Int]
findLocationsBy substr world = _locationId <$> locationsBy substr world

locationsBy :: Text -> World -> [Location]
locationsBy substr world = filter (T.isInfixOf (T.toLower substr) . T.toLower . genericShowt) (_locationEvents world ^.. folded)

sharePrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
sharePrefix l1 l2 = let prefix = map fst $ takeWhile (uncurry (==)) $ zip l1 l2
                        f = drop $ length prefix
                     in (prefix, f l1, f l2)

listToPairs :: [a] -> [(a,a)]
listToPairs list = fmap unwrap . filterDirs . scanToPairs $ list
  where filterDirs = filter (\(l, r) -> isJust l && isJust r)
        unwrap (Just left, Just right) = (left, right)
        scanToPairs = scanl (\acc item -> (snd acc, Just item)) (Nothing, Nothing)

zonePath :: World -> Int -> [Int]
zonePath world anyZoneLocId = concat $  mergeTree $ listToPairs (reverse . fmap fst . unLPath <$> MST.msTreeAt anyZoneLocId zone)
  where zone = zoneMap world anyZoneLocId
        mergeTree = foldr (\(l, r) acc -> mergePathPair l r : acc) []
        mergePathPair l r = let (commonPath, lPrivatePath, rPrivatePath) = sharePrefix l r
                                lastCommonNode = maybeToList . head . reverse $ commonPath
                                privatePathBack [] = []
                                privatePathBack (xs) = L.tail $ reverse $ lastCommonNode ++ xs
                                in concat [ privatePathBack lPrivatePath, rPrivatePath ]
