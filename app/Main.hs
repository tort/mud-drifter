module Main where

import Lib
import Pipes.Concurrent
import Pipes.Network.TCP
import Pipes.ByteString
import Pipes
import Data.Monoid
import System.IO (openFile, IOMode(WriteMode), hClose)

main :: IO ()
main = do (socket, addr) <- connectSock "bylins.su" "4000"
          (outLog, inLog) <- bsBox
          (outConsole, inConsole) <- bsBox
          logFile <- openFile "log" WriteMode
          forkIO $ do runEffect $ fromSocket socket (2^15) >-> toOutput (outConsole <> outLog)
                      performGC
          forkIO $ do runEffect $ fromInput inConsole >-> stdout
          forkIO $ do runEffect $ fromInput inLog >-> toHandle logFile
          runEffect $ stdin >-> toSocket socket
          hClose logFile

bsBox :: IO (Output ByteString, Input ByteString)
bsBox = spawn unbounded
