{-# LANGUAGE OverloadedStrings #-}

module MapperSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Mapper
import ServerInputParser
import System.IO
import Pipes
import Pipes.ByteString hiding (head)
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph as G
import Data.Functor.Identity
import Data.List as L
import Data.Functor
import Data.Text as T
import Data.Set
import qualified Data.Set as S
import Event
import World

spec :: Spec
spec = describe "Mapper" $ do
  it "can find path from any location to any location" pending
  it "map world data to graph" pending
  it "fold move events to directions" $ foldToDirectionsProperty moveEvents
    where moveEvents = [ MoveEvent "north"
                       , LocationEvent (Location (LocationId 2) (LocationTitle "2")) [] []
                       , LocationEvent (Location (LocationId 1) (LocationTitle "1")) [] []
                       , MoveEvent "north"
                       , LocationEvent (Location (LocationId 1) (LocationTitle "1")) [] []
                       , MoveEvent "north"
                       , LocationEvent (Location (LocationId 2) (LocationTitle "2")) [] []
                       , MoveEvent "north"
                       , LocationEvent (Location (LocationId 2) (LocationTitle "2")) [] []
                       , MoveEvent "north"
                       , LocationEvent (Location (LocationId 2) (LocationTitle "2")) [] []
                       , MoveEvent "north"
                       , LocationEvent (Location (LocationId 3) (LocationTitle "3")) [] []
                       ]

foldToDirectionsProperty :: [ServerEvent] -> Expectation
foldToDirectionsProperty moveEvents = do L.length directions `shouldBe` 4
                                           where toOpt = Just . Right
                                                 directions = runIdentity $ foldToDirections S.empty $ each (toOpt <$> moveEvents)
