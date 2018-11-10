{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CommandExecutor where

import Protolude
import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.Concurrent as PC
import Pipes.Safe
import Pipes.Concurrent hiding (send)
import Pipes.Network.TCP
import Network.Simple.TCP
import Data.Text
import Event

commandExecutor :: MonadSafe m => Output Event -> Pipe Event Event m ()
commandExecutor toDrifterBoxOutput = executeDisconnected
  where executeDisconnected = await >>= \evt -> case evt of (UserCommand Connect) -> do yield $ ConsoleOutput "connecting...\n>"
                                                                                        (sock, _) <- connectSock "bylins.su" "4000"
                                                                                        liftIO $ async $ runServerReader sock
                                                                                        yield $ ConsoleOutput "connected\n>"
                                                                                        executeConnected sock
                                                            (UserCommand Zap) -> do yield $ notConnectedOutput
                                                                                    executeDisconnected
                                                            ServerIOException -> do yield $ ConsoleOutput "connection broken\n>"
                                                                                    executeDisconnected
                                                            ServerClosedChannel -> do yield $ ConsoleOutput "disconnected\n>"
                                                                                      executeDisconnected
                                                            (ConsoleOutput _) -> do yield evt
                                                                                    executeDisconnected
                                                            PulseEvent -> do yield evt
                                                                             executeDisconnected
                                                            _ -> do yield notConnectedOutput
                                                                    executeDisconnected
        executeConnected sock = do event <- await
                                   yield event
                                   case event of (SendToServer text) -> do liftIO $ sendCommand sock text
                                                                           executeConnected sock
                                                 ServerClosedChannel -> do yield $ ConsoleOutput "disconnected\n"
                                                                           executeDisconnected
                                                 ServerIOException -> do yield $ ConsoleOutput "connection broken\n"
                                                                         executeDisconnected
                                                 (UserCommand Zap) -> do yield $ ConsoleOutput "closing socket...\n"
                                                                         closeSock sock
                                                                         yield $ ConsoleOutput "socket closed\n"
                                                                         executeDisconnected
                                                 (UserCommand Connect) -> do yield $ ConsoleOutput "already connected\n"
                                                                             executeConnected sock
                                                 _ -> executeConnected sock

        runServerReader sock = runSafeT $ runEffect $ safeServerReader sock >-> toOutput toDrifterBoxOutput >> fireServerClosedChannelEvent
        safeServerReader sock = (fromSocket sock (2^15) >-> PP.map ServerInput) `catchP` (\(SomeException e) -> yield ServerIOException)
        fireServerClosedChannelEvent = liftIO $ async $ atomically $ PC.send toDrifterBoxOutput ServerClosedChannel

notConnectedOutput = ConsoleOutput "not connected to server\n>"


sendCommand :: Socket -> Text -> IO ()
sendCommand sock txt = do send sock $ encodeUtf8 $ snoc txt '\n'
                          return ()
