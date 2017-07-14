{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Pipes.Concurrent

import Person
import Parser
import Console
import RemoteConsole

main :: IO ()
main = runWithLog

runWithLog :: IO ()
runWithLog = do 
    consoleBox <- spawn unbounded
    personBox <- spawn unbounded
    remoteConsoleBox <- spawn unbounded
    let consolePersonBB = (consoleBox, personBox)
    network <- compile $ keepConnectedTask consolePersonBB $ fst remoteConsoleBox
    actuate network
    runRemoteConsole (fst personBox) (snd remoteConsoleBox)
    runConsole (fst personBox) (snd consoleBox)
