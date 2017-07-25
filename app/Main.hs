{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Pipes.Concurrent
import Data.Monoid

import Person
import Parser
import Console
import RemoteConsole

main :: IO ()
main = runWithLog

runWithLog :: IO ()
runWithLog = do 
    (toRemoteConsole, runRemoteConsole) <- initRemoteConsole
    (toConsole, runConsole) <- initConsole
    toPerson <- runPerson $ toConsole <> toRemoteConsole

    runRemoteConsole toPerson 
    runConsole toPerson
