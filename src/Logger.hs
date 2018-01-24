{-# LANGUAGE OverloadedStrings #-}

module Logger ( runLogger ) where

import Pipes
import Pipes.Prelude
import Pipes.Concurrent
import qualified Pipes.Prelude as PP
import Event
import System.IO
import qualified System.IO as IO
import Prelude
import qualified Prelude as P
import Control.Concurrent.Async
import Control.Exception.Safe

runLogger :: Input Event -> IO ()
runLogger evtBusInput = do
  bracket
    (IO.openFile "evt.log" IO.WriteMode)
    (\h -> IO.hClose h)
    (\h -> do async $ do runEffect $ fromInput evtBusInput >-> PP.map P.show >-> PP.toHandle h
              return ())
