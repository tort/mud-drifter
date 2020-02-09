{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Protolude
import Pipes

import ServerInputParser
import Console
import Event
import Mapper
import Person
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import World

main :: IO ()
main = return ()
type RoomId = Int

genod = Person { personName = "генод"
               , personPassword = "каркасный"
               , residence = MudServer "bylins.su" 4000
               }

{-
runDrifter :: IO ()
runDrifter = do currentDir <- getCurrentDirectory
                world <- liftIO $ loadWorld currentDir
                evtLog <- openFile "evt.log" WriteMode
                toConsoleBox <- spawn $ newest 100
                toRemoteConsoleBox <- spawn $ newest 100
                toServerInputLoggerBox <- spawn $ newest 100
                toEvtLoggerBox <- spawn $ newest 100
                toDrifterBox <- spawn $ newest 100
                let commonOutput = (fst toConsoleBox) `mappend` (fst toRemoteConsoleBox) `mappend` (fst toServerInputLoggerBox) `mappend` (fst toEvtLoggerBox)
                    readConsoleInput = runEffect $ runSafeP $ consoleInput `catchP` onUserInputException >-> toOutput (fst toDrifterBox)
                    printConsoleOutput = runEffect $ (printWorldStats world >> fromInput (snd toConsoleBox)) >-> consoleOutput
                    runDrifter = runEffect $ runSafeP $ fromInput (snd toDrifterBox) >-> drifter world >-> commandExecutor (fst toDrifterBox) >-> (toOutput commonOutput)
                    emitPulseEvery = atomically $ PC.send (fst toDrifterBox) PulseEvent >> return ()
                    onUserInputException (SomeException e) = yield UserInputIOException
                    {-onUserInputException (SomeException e) = liftIO $ do hFlush serverInputLog
                                                                hClose serverInputLog
                                                                hFlush evtLog
                                                                hClose evtLog-}

                async $ runServerInputLogger (snd toServerInputLoggerBox)
                async $ runEvtLogger (snd toEvtLoggerBox) evtLog
                async $ printConsoleOutput
                async $ runDrifter
                async $ runRemoteConsole (fst toDrifterBox, snd toRemoteConsoleBox)
                repeatedTimer emitPulseEvery (sDelay 1)
                readConsoleInput
-}
