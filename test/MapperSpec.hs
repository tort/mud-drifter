{-# LANGUAGE OverloadedStrings #-}

module MapperSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances
import Mapper
import Person
import Parser
import System.IO
import Pipes
import Pipes.ByteString hiding (head)
import Data.Graph.Inductive.Graph
import Data.Functor.Identity
import Data.List as DL
import Data.Functor
import Data.Text as T

spec :: Spec
spec = describe "Mapper" $ do
  it "can find path from any location to any location" pending
  it "fold move events to graph properties" pending
  it "fold move events to graph" $ foldToGraphProperty moveEvents
      where moveEvents = [Move "north" $ LocData 2 "2"
                         , Location $ LocData 1 "1"
                         , Move "north" $ LocData 1 "1"
                         , Move "north" $ LocData 2 "2"
                         , Move "north" $ LocData 2 "2"
                         , Move "north" $ LocData 2 "2"
                         , Move "north" $ LocData 3 "3"
                         ]

foldToGraphProperty :: [ServerEvent] -> Expectation
foldToGraphProperty moveEvents = do size graph `shouldBe` 2
                                    order graph `shouldBe` 3
                                    where toOpt = Just . Right
                                          isMove (Move _ _) = True
                                          isMove _ = False
                                          graph = runIdentity $ foldToGraph $ each (toOpt <$> moveEvents)
