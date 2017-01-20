{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Control.Applicative ((<$))
import Pipes.Concurrent
import Pipes.Network.TCP
import Pipes.ByteString as BS
import Pipes.Text.IO as PT
import Data.Text.Encoding (encodeUtf8)
import Data.Text as TE
import qualified Pipes.Prelude as PP
import Pipes.Prelude.Text as Text (stdinLn)
import Pipes
import Data.Monoid
import System.IO as SIO (withFile, IOMode(WriteMode))

main :: IO ()
main = SIO.withFile "log" WriteMode $ \logFile ->
            connect "bylins.su" "4000" $ \(socket, addr) ->
            do (outLog, inLog) <- spawn unbounded
               (outConsole, inConsole) <- spawn unbounded
               forkIO $ do runEffect $ fromSocket socket (2^15) >-> toOutput (outConsole <> outLog)
                           performGC
               forkIO $ do runEffect $ fromInput inConsole >-> BS.stdout
                           performGC
               forkIO $ do runEffect $ fromInput inLog >-> BS.toHandle logFile
                           performGC
               runEffect $ Text.stdinLn >-> PP.takeWhile(/= ":quit") >-> PP.map (\x -> append x "\n") >-> PP.map encodeUtf8  >-> toSocket socket
