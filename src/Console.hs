{-# LANGUAGE OverloadedStrings #-}

module Console (
  runConsole
) where

import Person
import Pipes.Concurrent
import qualified Pipes.Prelude as PPR
import qualified Pipes.Prelude.Text as PPT
import Pipes
import qualified Data.Text as TE
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as DTIO

type SendAction = (ConsoleCommand -> IO ())

runConsole :: SendAction -> IO ()
runConsole sendAction = do
  --liftIO $ sendAction $ SetSendToConsoleAction DTIO.putStrLn
  runEffect $ PPT.stdinLn >-> PPR.takeWhile(/= ":quit") >-> sendToPersonConsumer sendAction
  return ()

sendToPersonConsumer :: SendAction -> Consumer TE.Text IO ()
sendToPersonConsumer sendAction = do
  text <- await
  liftIO $ sendAction $ UserInput text
  sendToPersonConsumer sendAction
