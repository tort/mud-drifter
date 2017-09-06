{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Pipes.Concurrent
import Data.Monoid
import Data.ByteString
import qualified Pipes.Concurrent as PC

import Person
import ServerInputParser
import Console
import RemoteConsole

main :: IO ()
main = runWithLog

runWithLog :: IO ()
runWithLog = do
    world <- loadWorld "/Users/anesterov/workspace/mud-drifter/archive/"
    (toRemoteConsole, runRemoteConsole) <- initRemoteConsole
    (toConsole, runConsole) <- initConsole
    toPerson <- runPerson world $ sendToConsoles $ toConsole <> toRemoteConsole

    runRemoteConsole toPerson
    runConsole toPerson

sendToConsoles :: Output ByteString -> SendToConsolesAction
sendToConsoles channel msg = do atomically $ PC.send channel msg
                                return ()
