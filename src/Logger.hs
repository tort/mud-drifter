{-# LANGUAGE OverloadedStrings #-}

module Logger ( runLogger
              , readLog
              , printEvents
              , printQuestEvents
              ) where

import Pipes
import Pipes.Prelude hiding (mapM_)
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
import qualified Data.ByteString.Char8 as DBC8
import qualified Data.Text.IO as DTIO
import Data.Monoid

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

printEvents :: [Event] -> IO ()
printEvents events = mapM_ printEvent events
  where printEvent (ServerEvent (UnknownServerEvent txt)) = DBC8.putStrLn ("UnknownServerEvent: " <> txt <> "\ESC[0m")
        printEvent (ConsoleInput txt) = DTIO.putStrLn ("ConsoleInput: " <> txt <> "\ESC[0m")
        printEvent event = IO.print event

printQuestEvents :: [Event] -> IO ()
printQuestEvents events = printEvents $ P.filter questEvents events
  where questEvents e = (not $ isServerInput e)
                        && (not $ isMoveEvent e)
                        && (e /= (ServerEvent PromptEvent))
                        && (not $ emptyUnknownServerEvent e)
                        && (not $ isConsoleOutput e)
        isServerInput (ServerInput _) = True
        isServerInput _ = False
        isMoveEvent (ServerEvent (MoveEvent _)) = True
        isMoveEvent _ = False
        emptyUnknownServerEvent (ServerEvent (UnknownServerEvent "")) = True
        emptyUnknownServerEvent _ = False
        isConsoleOutput (ConsoleOutput _) = True
        isConsoleOutput _ = False
