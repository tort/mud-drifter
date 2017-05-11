{-# LANGUAGE OverloadedStrings #-}

module Mapper (
  countRoomsSimple
  , countRoomsInGraph
) where

import qualified Data.ByteString.Char8 as BS
import Data.Graph.Inductive
import System.IO
import Prelude as P
import Data.Char

data Loc = Loc
type MNode = LNode Loc
type MEdge = LEdge Int

parseLog :: Handle -> Gr MNode MEdge
parseLog file = undefined

countRoomsSimple :: BS.ByteString -> Int
countRoomsSimple str = length $ filterLocTitles $ BS.lines str

locColorMarker :: BS.ByteString
locColorMarker = "\ESC[1;36m"

countRoomsInGraph :: BS.ByteString -> Int
countRoomsInGraph str = 0

startWithLocColor :: BS.ByteString -> Bool
startWithLocColor line = BS.isPrefixOf locColorMarker line

filterLocTitles :: [BS.ByteString] -> [BS.ByteString]
filterLocTitles lines = P.filter (\line -> startWithLocColor line) lines
