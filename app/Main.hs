{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import Pipes
import Pipes.Concurrent

import ServerInputParser
import Console
import RemoteConsole
import Logger
import Drifter
import Event
import CommandExecutor
import Mapper
import Person

main :: IO ()
main = runDrifter

runDrifter :: IO ()
runDrifter = do toConsoleBox <- spawn $ newest 10
                toLoggerBox <- spawn $ newest 10
                toDrifterBox <- spawn unbounded
                let commonOutput = (fst toConsoleBox) `mappend` (fst toLoggerBox)
                    readConsoleInput = runEffect $ consoleInput >-> toOutput (fst toDrifterBox)
                    printConsoleOutput = runEffect $ fromInput (snd toConsoleBox) >-> consoleOutput
                    runDrifter = runEffect $ fromInput (snd toDrifterBox) >-> drifter >-> commandExecutor (fst toDrifterBox) >-> (toOutput commonOutput)

                async $ runLogger (snd toLoggerBox)
                async $ printConsoleOutput
                async $ runDrifter
                readConsoleInput
