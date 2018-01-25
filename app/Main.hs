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
import Logger
import Control.Concurrent.Async
import Pipes
import Control.Monad (forever)

main :: IO ()
main = runDrifter

runDrifter :: IO ()
runDrifter = do
    world <- loadWorld "/Users/anesterov/workspace/mud-drifter/archive/"
    consoleBranch <- spawn $ newest 1024
    remoteConsoleBranch <- spawn $ newest 1024
    personBranch <- spawn $ newest 1024
    loggerBranch <- spawn $ newest 1024
    let toEvtBus = fst personBranch <> (fst consoleBranch) <> (fst loggerBranch)

    runPerson world (toEvtBus, snd personBranch)
    runLogger $ snd loggerBranch
    --runRemoteConsole toEvtBus (snd remoteConsoleBranch)
    runConsole toEvtBus (snd consoleBranch)
