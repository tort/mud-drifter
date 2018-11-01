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
import Pipes.Safe

main :: IO ()
main = runDrifter

runDrifter :: IO ()
runDrifter = do toConsoleBox <- spawn $ newest 100
                toServerInputLoggerBox <- spawn $ newest 100
                toEvtLoggerBox <- spawn $ newest 100
                toDrifterBox <- spawn $ newest 100
                let commonOutput = (fst toConsoleBox) `mappend` (fst toServerInputLoggerBox) `mappend` (fst toEvtLoggerBox)
                    readConsoleInput = runEffect $ consoleInput >-> toOutput (fst toDrifterBox)
                    printConsoleOutput = runEffect $ fromInput (snd toConsoleBox) >-> consoleOutput
                    runDrifter = runEffect $ runSafeP $ fromInput (snd toDrifterBox) >-> drifter >-> commandExecutor (fst toDrifterBox) >-> (toOutput commonOutput)

                async $ runServerInputLogger (snd toServerInputLoggerBox)
                async $ runEvtLogger (snd toEvtLoggerBox)
                async $ printConsoleOutput
                async $ runDrifter
                readConsoleInput
