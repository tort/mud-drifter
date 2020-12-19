{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Console ( consoleInput
               , consoleOutput
               , filterConsoleOutput
               ) where

import           Control.Concurrent.Async
import           Control.Monad            (forever)
import qualified Data.ByteString.Char8    as DBC8
import           Data.Text
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO             as DTIO
import           Event
import           Pipes
import qualified Pipes.ByteString         as PBS
import           Pipes.Concurrent
import qualified Pipes.Prelude            as PP
import           Protolude                hiding (yield)
import           Text.Parsec
import qualified Text.Parsec              as Parsec

consoleInput :: Monad m => Producer Event m ()
consoleInput = return ()
--consoleInput = PPT.stdinLn >-> PP.takeWhile(/= "/quit") >-> PP.map ConsoleInput >> liftIO (DTIO.putStr "console input stream finished\n")

consoleOutput :: Consumer Event IO ()
consoleOutput =
  filterConsoleOutput >-> PBS.stdout >>
  liftIO (DTIO.putStr "console receive stream finished\n")

filterConsoleOutput :: Pipe Event ByteString IO ()
filterConsoleOutput = forever $ do evt <- await
                                   case evt of (ConsoleOutput text) -> yield text
                                               (ServerInput text) -> yield text
                                               e -> return ()
