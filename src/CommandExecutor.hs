{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module CommandExecutor where

import Protolude hiding (yield)
import Pipes
import qualified Pipes.Prelude as PP
import qualified Pipes.Concurrent as PC
import Pipes.Concurrent hiding (send)
import Pipes.Network.TCP
import Network.Simple.TCP
import Data.Text
import Data.Heap
import qualified Data.Heap as H
import Event

commandExecutor :: MonadIO m => Pipe Event ByteString m ()
commandExecutor = exec H.empty
  where
    exec :: MonadIO m => MaxPrioHeap Int Text -> Pipe Event ByteString m ()
    exec heap =
      await >>= \case
        (SendToServer text) ->
          (yield . renderCommand $ text) >> (liftIO $ putStrLn text) >>
          exec heap
        event@(SendOnPulse prio text)  -> exec (insert (prio, text) heap)
        PulseEvent ->
          case view heap of
            Nothing -> exec heap
            (Just ((_, text), rest)) ->
              (liftIO $ putStrLn text) >> (yield . renderCommand $ text) >>
              exec rest
{-
  forever $
  await >>= \case
    (SendToServer text) ->
      (liftIO $ putStrLn text) >> (yield $ encodeUtf8 $ snoc text '\n')
    _ -> return ()
-}

renderCommand text = encodeUtf8 $ snoc text '\n'

sendCommand :: Socket -> Text -> IO ()
sendCommand sock txt = do send sock $ encodeUtf8 $ snoc txt '\n'
                          return ()
