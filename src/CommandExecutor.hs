{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module CommandExecutor where

import Protolude
import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.Concurrent as PC
import Pipes.Concurrent hiding (send)
import Pipes.Network.TCP
import Network.Simple.TCP
import Data.Text
import Event

commandExecutor :: Monad m => Pipe Event ByteString m ()
commandExecutor = forever $ await >>= \case (SendToServer text) -> yield $ encodeUtf8 $ snoc text '\n'
                                            _ -> return ()

sendCommand :: Socket -> Text -> IO ()
sendCommand sock txt = do send sock $ encodeUtf8 $ snoc txt '\n'
                          return ()
