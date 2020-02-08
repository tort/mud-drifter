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
import Event
import CommandExecutor
import Mapper
import Person
import qualified Pipes.Concurrent as PC
import qualified Pipes.Prelude as PP
import Pipes.Network.TCP
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
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

withPerson :: (Output ByteString, Input Event) -> Pipe Event Event IO () -> IO ()
withPerson channels task = runEffect $ fromInput (snd channels) >-> task >-> commandExecutor >-> toOutput (fst channels)

initPerson :: Person -> IO (Output ByteString, Input Event)
initPerson person = do
  toServerBox <- spawn $ newest 100
  toDrifterBox <- spawn $ newest 100
  (outToLoggerBox, inToLoggerBox, sealToLoggerBox) <- spawn' $ newest 100
  async $ connect mudHost mudPort $ \(sock, _) -> do
    toServerInputParserBox <- spawn $ newest 100
    print "connected"
    toRemoteConsoleBox <- spawn $ newest 100
    let commonOutput = (fst toRemoteConsoleBox) `mappend` (fst toServerInputParserBox) `mappend` outToLoggerBox
        emitPulseEvery = atomically $ PC.send (fst toDrifterBox) PulseEvent >> return ()
    async $ runRemoteConsole (fst toServerBox, snd toRemoteConsoleBox)
    async $ runEffect $ fromInput (snd toServerBox) >-> toSocket sock
    async $ runEffect $ parseServerEvents (fromInput (snd toServerInputParserBox)) >-> PP.map ServerEvent >-> toOutput (fst toDrifterBox) >>= liftIO . print
    async $ runServerInputLogger inToLoggerBox
    repeatedTimer emitPulseEvery (sDelay 1)
    runEffect $ fromSocket sock (2^15) >-> toOutput commonOutput >> (liftIO $ print "remote connection closed")
    performGC
    atomically sealToLoggerBox
    print "disconnected"
  return (fst toServerBox, snd toDrifterBox)
    where mudHost = T.unpack . host . residence $ person
          mudPort = show . port . residence $ person

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
