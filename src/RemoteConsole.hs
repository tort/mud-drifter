{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RemoteConsole ( runRemoteConsole ) where

import Protolude hiding (yield, traceIO)
import Pipes.Concurrent hiding (send)
import qualified Pipes.Prelude as PP
import qualified Pipes.Attoparsec as PA
import Pipes
import qualified Pipes.Parse as PP
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Pipes.Network.TCP
import Control.Concurrent.Async
import Data.ByteString.Char8
import qualified Data.ByteString.Char8 as C8
import Pipes.Attoparsec
import ServerInputParser
import Debug.Trace
import Event
import Console
import Control.Monad
import Pipes.Safe

runRemoteConsole :: (Output Event, Input ByteString) -> IO ()
runRemoteConsole (evtBusOutput, evtBusInput) = do
  listen (Host "0.0.0.0") "4000" $ \(sock, addr) -> do
    accept sock $ \(s, _) -> do
      async $
        runEffect $
          runSafeP $
            readSockPipeM s evtBusOutput
      runEffect $
        fromInput evtBusInput >-> toSocket s *>
        liftIO (print "server -> remote console stream finished") *>
        closeSock s
      return ()
    return ()
  return ()

readSockPipe :: Socket -> Output Event -> Effect IO ()
readSockPipe s out = telnetFilteringParser (fromSocket s (2 ^ 15)) >-> PP.map ConsoleInput >-> toOutput out

readSockPipeM :: (MonadSafe m, MonadIO m) => Socket -> Output Event -> Effect m ()
readSockPipeM s out = catchP (telnetFilteringParser (fromSocket s (2 ^ 15))) onUserInputException >-> PP.map ConsoleInput >-> toOutput out >> liftIO (print "read from remote console finished")

onUserInputException (SomeException e) = void (liftIO (print "socket exception"))

telnetFilteringParser :: MonadIO m => Producer ByteString m () -> Producer ByteString m ()
telnetFilteringParser src = PA.parsed remoteInputParser src >>= onEndOrError
  where onEndOrError Right{} = liftIO $ print "remote console input parsing finished"
        onEndOrError (Left (err, producer)) = liftIO $ print "error when parsing remote input"
        --errDesc (ParsingError ctxts msg) = "error: " <> C8.pack msg <> C8.pack (C8.concat ctxts) <> "\n"


traceBS :: Pipe ByteString ByteString IO ()
traceBS = do msg <- await
             liftIO $ traceIO $ "message from remote console: " ++ C8.unpack msg
             yield msg
             traceBS
