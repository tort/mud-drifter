{-# LANGUAGE OverloadedStrings #-}

module Console (
  runConsole
) where

import Person
import Pipes.Concurrent
import qualified Pipes.Prelude as PPR
import qualified Pipes.Prelude.Text as PPT
import Pipes
import qualified Pipes.Prelude as PP
import qualified Data.Text as TE
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as DTIO
import Pipes.Network.TCP
import Pipes.Concurrent
import Control.Concurrent.Async
import qualified Data.ByteString as BS
import qualified Pipes.ByteString as PB

runConsole :: Output TE.Text -> Input PB.ByteString -> IO ()
runConsole sendChan receiveChan = do
  async $ do runEffect $ fromInput receiveChan >-> PB.stdout >> (liftIO $ BS.putStr "console receive stream finished")
             performGC
  runEffect $ PPT.stdinLn >-> PPR.takeWhile(/= ":quit") >-> toOutput sendChan >> (liftIO $ BS.putStr "console send stream finished")
