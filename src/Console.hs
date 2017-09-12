{-# LANGUAGE OverloadedStrings #-}

module Console ( runConsole
               , filterConsoleOutput
               ) where

import Event
import Pipes
import Pipes.Concurrent
import qualified Pipes.Concurrent as PC
import qualified Pipes.Prelude as PP
import qualified Pipes.Prelude.Text as PPT
import qualified Pipes.Text.IO as PTIO
import qualified Data.Text as TE
import qualified Pipes.ByteString as PB
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as DTIO
import Control.Concurrent.Async
import Text.Parsec
import qualified Text.Parsec as Parsec
import Data.ByteString.Char8 as DBC8 hiding (isInfixOf, isPrefixOf, snoc, putStrLn)
import UserInputParser
import Control.Monad (forever)

runConsole :: Output Event -> Input Event -> IO ()
runConsole evtBusOutput evtBusInput = do
  async $ do runEffect $ fromInput evtBusInput >-> filterConsoleOutput >-> PB.stdout >> liftIO (DTIO.putStr "console receive stream finished")
             performGC
  runEffect $ PPT.stdinLn >-> PP.takeWhile(/= ":quit") >-> PP.map Event.ConsoleInput >-> toOutput evtBusOutput >> liftIO (DTIO.putStr "console send stream finished")

filterConsoleOutput :: Pipe Event ByteString IO ()
filterConsoleOutput = forever $ do evt <- await
                                   case evt of (Event.ConsoleOutput text) -> yield text
                                               (Event.ServerInput text) -> yield text
                                               e -> return ()
