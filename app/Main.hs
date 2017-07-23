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
    consoleBox <- spawn unbounded
    remoteConsoleBox <- spawn unbounded
    
    (toPerson, runPerson) <- initPerson
    runPerson $ fst consoleBox <> fst remoteConsoleBox

    runRemoteConsole (snd remoteConsoleBox) toPerson 
    runConsole toPerson (snd consoleBox)
