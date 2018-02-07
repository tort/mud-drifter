{-# LANGUAGE OverloadedStrings #-}

module Logger ( runLogger
              , readLog
              ) where

import Pipes
import Pipes.Prelude
import Pipes.Concurrent
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PBS
import Event
import qualified Event as E
import System.IO
import qualified System.IO as IO
import Prelude
import qualified Prelude as P
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad
import Data.Maybe
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy as BSL

runLogger :: Input Event -> IO ()
runLogger evtBusInput = do async $ (bracketWithError
                                    (IO.openFile "evt.log" IO.WriteMode)
                                    (\e h -> do P.putStrLn $ P.show e
                                                IO.hClose h)
                                    (\h -> runEffect $ fromInput evtBusInput >-> PP.map (toStrict . encode) >-> PBS.toHandle h >> liftIO (IO.putStr "logger input stream ceased")))
                           return ()

readLog :: String -> IO [E.Event]
readLog filePath = do contents <- BSL.readFile filePath
                      return $ parse contents
                        where parse "" = []
                              parse input = let (Right (rem, _, evt)) = decodeEvent input
                                             in evt : parse rem
                                            where decodeEvent input = decodeOrFail input :: Either (BSL.ByteString, ByteOffset, String) (BSL.ByteString, ByteOffset, E.Event)
