{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

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
import qualified Pipes.Prelude as PP
import Pipes.Network.TCP
import qualified Pipes.ByteString as PBS
import qualified Data.ByteString.Char8 as C8
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

runGenod :: Pipe Event Event IO () -> IO ()
runGenod task = runPerson genod task

runPerson :: Person -> Pipe Event Event IO () -> IO ()
runPerson person task = initPerson person >>= \(o, i) ->
  runEffect $ fromInput i >-> parseServerInputPipe >-> task >-> commandExecutor >-> toOutput o

withPerson :: (Output ByteString, Input Event) -> Pipe Event Event IO () -> IO ()
withPerson channels task = runEffect $ fromInput (snd channels) >-> parseServerInputPipe >-> task >-> commandExecutor >-> toOutput (fst channels)

initPerson :: Person -> IO (Output ByteString, Input Event)
initPerson person = do
  toServerBox <- spawn $ newest 100
  toDrifterBox <- spawn $ newest 100
  async $ connect "bylins.su" "4000" $ \(sock, _) -> do
    print "connected"
    toRemoteConsoleBox <- spawn $ newest 100
    let commonOutput = (fst toRemoteConsoleBox) `mappend` (fst toDrifterBox)
        emitPulseEvery = atomically $ PC.send (fst toDrifterBox) PulseEvent >> return ()
    async $ runRemoteConsole (fst toServerBox, snd toRemoteConsoleBox)
    async $ runEffect $ fromInput (snd toServerBox) >-> toSocket sock
    repeatedTimer emitPulseEvery (sDelay 1)
    runEffect $ fromSocket sock (2^15) >-> PP.map ServerInput >-> toOutput commonOutput >> (liftIO $ print "remote connection closed")
    print "disconnected"
  return (fst toServerBox, snd toDrifterBox)

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
