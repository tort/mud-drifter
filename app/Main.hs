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
                toServerInputLoggerBox <- spawn $ newest 10
                toEvtLoggerBox <- spawn $ newest 10
                toDrifterBox <- spawn unbounded
                let commonOutput = (fst toConsoleBox) `mappend` (fst toServerInputLoggerBox) `mappend` (fst toEvtLoggerBox)
                    readConsoleInput = runEffect $ consoleInput >-> toOutput (fst toDrifterBox)
                    printConsoleOutput = runEffect $ fromInput (snd toConsoleBox) >-> consoleOutput
                    runDrifter = runEffect $ fromInput (snd toDrifterBox) >-> drifter >-> commandExecutor (fst toDrifterBox) >-> (toOutput commonOutput)

                async $ runServerInputLogger (snd toServerInputLoggerBox)
                async $ runEvtLogger (snd toEvtLoggerBox)
                async $ printConsoleOutput
                async $ runDrifter
                readConsoleInput
