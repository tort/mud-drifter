{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Pipes.Concurrent
import Data.Monoid
import Data.ByteString
import qualified Pipes.Concurrent as PC

import Person
import Parser
import Console
import RemoteConsole

main :: IO ()
main = runWithLog

runWithLog :: IO ()
runWithLog = do
    worldMap <- loadMap "/Users/anesterov/workspace/mud-drifter/archive/"
    (toRemoteConsole, runRemoteConsole) <- initRemoteConsole
    (toConsole, runConsole) <- initConsole
    toPerson <- runPerson worldMap $ sendToConsoles $ toConsole <> toRemoteConsole

    runRemoteConsole toPerson
    runConsole toPerson

sendToConsoles :: Output ByteString -> SendToConsolesAction
sendToConsoles channel msg = do atomically $ PC.send channel msg
                                return ()
