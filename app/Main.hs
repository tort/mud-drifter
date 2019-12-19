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
import System.Directory
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
import World

main :: IO ()
main = return ()

type Name = Text
type Password = Text
type RoomId = Int

data MudServer = MudServer { host :: Text
                           , port :: Int
                           } deriving (Eq, Show)

data Person = Person { personName :: Name
                     , personPassword :: Password
                     , residence :: MudServer
                     } deriving (Eq, Show)

genod = Person { personName = "генод"
               , personPassword = "каркасный"
               , residence = MudServer "bylins.su" 4000
               }

runGenod :: Pipe Event Event m ()
runGenod = undefined

go :: RoomId -> Person -> IO ()
go roomId person = undefined

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
