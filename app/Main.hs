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
import qualified Pipes.Concurrent as PC
import System.IO
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
import World

main :: IO ()
main = runDrifter

runDrifter :: IO ()
runDrifter = do world <- liftIO $ loadWorld "/Users/anesterov/workspace/mud-drifter/archive/"
                serverInputLog <- openFile "server-input.log" WriteMode
                evtLog <- openFile "evt.log" WriteMode
                toConsoleBox <- spawn $ newest 100
                toServerInputLoggerBox <- spawn $ newest 100
                toEvtLoggerBox <- spawn $ newest 100
                toDrifterBox <- spawn $ newest 100
                let commonOutput = (fst toConsoleBox) `mappend` (fst toServerInputLoggerBox) `mappend` (fst toEvtLoggerBox)
                    readConsoleInput = runEffect $ runSafeP $ consoleInput `catchP` onException >-> toOutput (fst toDrifterBox)
                    printConsoleOutput = runEffect $ (printWorldStats world >> fromInput (snd toConsoleBox)) >-> consoleOutput
                    runDrifter = runEffect $ runSafeP $ fromInput (snd toDrifterBox) >-> drifter world >-> commandExecutor (fst toDrifterBox) >-> (toOutput commonOutput)
                    emitPulseEvery = atomically $ PC.send (fst toDrifterBox) PulseEvent >> return ()
                    onException (SomeException e) = liftIO $ do hFlush serverInputLog
                                                                hClose serverInputLog
                                                                hFlush evtLog
                                                                hClose evtLog

                async $ runServerInputLogger (snd toServerInputLoggerBox) serverInputLog
                async $ runEvtLogger (snd toEvtLoggerBox) evtLog
                async $ printConsoleOutput
                async $ runDrifter
                repeatedTimer emitPulseEvery (sDelay 1)
                readConsoleInput
