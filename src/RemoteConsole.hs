{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RemoteConsole ( runRemoteConsole ) where

import Prelude
import Pipes.Concurrent hiding (send)
import qualified Pipes.Prelude as PP
import qualified Pipes.Attoparsec as PA
import Pipes
import qualified Pipes.Parse as PP
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

runRemoteConsole :: (Output ByteString, Input ByteString) -> IO ()
runRemoteConsole (evtBusOutput, evtBusInput) = do
  serve (Host "0.0.0.0") "4000" $ \(sock, addr) -> do
    async $ runEffect $ fromInput evtBusInput >-> toSocket sock
    runEffect $ telnetFilteringParser (fromSocket sock (2^15)) >-> toOutput evtBusOutput >> (liftIO $ print "remote console input parsing finished")
    return ()
  return ()

extractText :: Pipe RemoteConsoleEvent ByteString IO ()
extractText = do evt <- await
                 handle evt
                 extractText
              where handle (RemoteUserInput txt) = yield txt
                    handle _ = return ()

telnetFilteringParser :: Producer ByteString IO () -> Producer ByteString IO ()
telnetFilteringParser src = PA.parsed remoteInputParser src >>= onEndOrError
  where onEndOrError Right{} = liftIO $ print "remote console input parsing finished"
        onEndOrError (Left (err, producer)) = (liftIO $ print "error when parsing remote input")
        --errDesc (ParsingError ctxts msg) = "error: " <> C8.pack msg <> C8.pack (C8.concat ctxts) <> "\n"


traceBS :: Pipe ByteString ByteString IO ()
traceBS = do msg <- await
             liftIO $ traceIO $ "message from remote console: " ++ C8.unpack msg
             yield msg
             traceBS
