{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Mapper ( mapper
              ) where

import Protolude hiding ((<>), Location, runStateT, head, intercalate)
import Data.ByteString.Char8()
import ServerInputParser
import Pipes
import Pipes.Parse
import qualified Pipes.Prelude as PP
import Data.Text
import Event
import World
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Graph.Inductive.Query.SP as GA

mapper :: MonadIO m => Pipe Event Event m ()
mapper = do world <- liftIO $ loadWorld "/Users/anesterov/workspace/mud-drifter/archive/"
            let mapperWithPosition currLoc = do evt <- await
                                                case evt of (ServerEvent (LocationEvent loc _)) -> mapperWithPosition (Just $ locId loc)
                                                            _ -> do yield $ case evt of (UserCommand (FindLoc text)) -> ConsoleOutput $ showLocs $ locsByRegex world text
                                                                                        (UserCommand (FindPathFromTo from to)) -> ConsoleOutput $ showPathBy world (Just from) to
                                                                                        (UserCommand (FindPathToLocId to)) -> ConsoleOutput $ showPathBy world currLoc to
                                                                                        (UserCommand (FindPathTo regex)) -> let matchingLocs = locsByRegex world regex
                                                                                                                             in ConsoleOutput $ case S.toList $ matchingLocs of
                                                                                                                                  [] -> "no matching locations found"
                                                                                                                                  d:[] -> showPathBy world currLoc (locId d)
                                                                                                                                  _ -> showLocs matchingLocs
                                                                                        x -> x
                                                                    mapperWithPosition currLoc
             in mapperWithPosition Nothing



showPath :: World -> Path -> ByteString
showPath world [] = "path is empty\n"
showPath world path = (encodeUtf8 . addRet . joinToOneMsg) (showDirection . (nodePairToDirection world) <$> toJust <$> nodePairs)
  where joinToOneMsg = intercalate "\n"
        showDirection = trigger
        addRet txt = snoc txt '\n'
        nodePairs = filterDirs $ L.scanl (\acc item -> (snd acc, Just item)) (Nothing, Nothing) path
        filterDirs = L.filter (\pair -> isJust (fst pair) && isJust (snd pair))
        toJust (Just left, Just right) = (left, right)

nodePairToDirection :: World -> (LocId, LocId) -> Direction
nodePairToDirection world (from, to) = L.head $ S.toList $ S.filter (\d -> locIdFrom d == from && locIdTo d == to) (directions world)

showPathBy :: World -> Maybe Int -> Int -> ByteString
showPathBy world Nothing _ = "current location is unknown\n"
showPathBy world (Just f) t = if (f == t) then "you are already there!"
                                          else (showPath $ world) $ GA.sp f t (worldMap world)

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
