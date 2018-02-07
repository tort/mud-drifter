{-# LANGUAGE OverloadedStrings #-}

module Logger ( runLogger ) where

import Pipes
import Pipes.Prelude
import Pipes.Concurrent
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PBS
import Event
import System.IO
import qualified System.IO as IO
import Prelude
import qualified Prelude as P
import Control.Concurrent.Async
import Control.Exception.Safe
import Control.Monad
import Data.Maybe
import Data.Binary
import Data.ByteString.Lazy (toStrict)

runLogger :: Input Event -> IO ()
runLogger evtBusInput = do async $ (bracketWithError
                                    (IO.openFile "evt.log" IO.WriteMode)
                                    (\e h -> do P.putStrLn $ P.show e
                                                IO.hClose h)
                                    (\h -> runEffect $ fromInput evtBusInput >-> PP.map (toStrict . encode) >-> PBS.toHandle h >> liftIO (IO.putStr "logger input stream ceased")))
                           return ()
