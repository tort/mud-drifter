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
import Event

commandExecutor :: MonadIO m => Pipe Event ByteString m ()
commandExecutor = exec Nothing
  where
    exec acc@Nothing =
      await >>= \case
        (SendToServer text) ->
          (yield . renderCommand $ text) >>
          (liftIO $ putStrLn text) >>
          exec acc
        event@SendOnPulse {} -> exec (Just event)
        _ -> exec acc
    exec acc@(Just (SendOnPulse textOnPulse)) =
      await >>= \case
        (SendToServer text) ->
          (yield . renderCommand $ text) >>
          (liftIO $ putStrLn text) >>
          exec acc
        event@SendOnPulse {} -> exec (Just event)
        PulseEvent ->
          (liftIO $ putStrLn textOnPulse) >>
          (yield . renderCommand $ textOnPulse) >>
          exec Nothing
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
