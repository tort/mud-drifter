{-# LANGUAGE OverloadedStrings #-}

module MapperSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Mapper
import Person
import ServerInputParser
import System.IO
import Pipes
import Pipes.ByteString hiding (head)
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph as G
import Data.Functor.Identity
import Data.List as DL
import Data.Functor
import Data.Text as T
import Data.Set
import qualified Data.Set as S
import Event

spec :: Spec
spec = describe "Mapper" $ do
  it "can find path from any location to any location" pending
  it "map world data to graph" pending
  it "fold move events to directions" $ foldToDirectionsProperty moveEvents
    where moveEvents = [ MoveEvent "north"
                       , LocationEvent $ Location 2 "2"
                       , LocationEvent $ Location 1 "1"
                       , MoveEvent "north"
                       , LocationEvent $ Location 1 "1"
                       , MoveEvent "north"
                       , LocationEvent $ Location 2 "2"
                       , MoveEvent "north"
                       , LocationEvent $ Location 2 "2"
                       , MoveEvent "north"
                       , LocationEvent $ Location 2 "2"
                       , MoveEvent "north"
                       , LocationEvent $ Location 3 "3"
                       ]

foldToDirectionsProperty :: [ServerEvent] -> Expectation
foldToDirectionsProperty moveEvents = do DL.length directions `shouldBe` 4
                                           where toOpt = Just . Right
                                                 isMove (MoveEvent _) = True
                                                 isMove _ = False
                                                 directions = runIdentity $ foldToDirections S.empty $ each (toOpt <$> moveEvents)
