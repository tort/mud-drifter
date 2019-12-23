{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module RemoteConsole ( runRemoteConsole ) where

import Prelude
import Pipes.Concurrent hiding (send)
import qualified Pipes.Prelude as PP
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

runRemoteConsole :: (Output ByteString, Input ByteString) -> IO ()
runRemoteConsole (evtBusOutput, evtBusInput) = do
  serve (Host "0.0.0.0") "4000" $ \(sock, addr) -> do
                                                async $ runEffect $ fromInput evtBusInput >-> toSocket sock
                                                runEffect $ (fromSocket sock (2^15)) >-> toOutput evtBusOutput
                                                return ()
  return ()

extractText :: Pipe RemoteConsoleEvent ByteString IO ()
extractText = do evt <- await
                 handle evt
                 extractText
              where handle (RemoteUserInput txt) = yield txt
                    handle _ = return ()

parseRemoteInput :: Socket -> Producer ByteString IO () -> Producer RemoteConsoleEvent IO ()
parseRemoteInput sock src = do
    (result, partial) <- liftIO $ PP.runStateT (parse remoteInputParser) src
    continue result partial
    where continue (Just (Left err)) _ = liftIO $ send sock "\nerror when parsing remote console input"
          continue Nothing _ = liftIO $ send sock "\nparsed entire stream"
          continue result@(Just (Right bs)) partial = do yield bs
                                                         parseRemoteInput sock partial

traceBS :: Pipe ByteString ByteString IO ()
traceBS = do msg <- await
             liftIO $ traceIO $ "message from remote console: " ++ C8.unpack msg
             yield msg
             traceBS
