{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Logger ( runLogger
              , withLog
              , printEvents
              , printQuestEvents
              , obstacleActions
              , filterQuestEvents
              , filterTravelActions
              ) where

import Protolude hiding ((<>), toStrict)
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PBS
import Event
import qualified System.IO as IO
import Control.Exception.Safe
import Control.Monad
import Data.String
import Data.Maybe
import Data.Either
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Monoid
import qualified Data.Foldable as F
import Data.Map.Strict hiding (foldl, foldl')
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.List as L

runLogger :: Input Event -> IO ()
runLogger evtBusInput = (bracketWithError
                                    (openFile "evt.log" IO.WriteMode)
                                    (\e h -> do LC8.putStrLn $ show e
                                                IO.hClose h)
                                    (\h -> runEffect $ fromInput evtBusInput >-> PP.filter serverInteractions >-> PP.map (encode) >-> toFile h >> liftIO (LC8.putStr "logger input stream ceased\n")))
  where toFile h = forever $ await >>= \s -> liftIO $ LC8.hPutStr h s

serverInteractions :: Event -> Bool
serverInteractions (SendToServer x) = True
serverInteractions (ServerEvent x) = True
serverInteractions (ServerInput x) = True
serverInteractions x = False

withLog :: String -> ([Event] -> r) -> IO r
withLog filePath action = withFile filePath ReadMode $ \handle ->
  action <$> parse . LC8.fromStrict <$> C8.hGetContents handle
    where parse "" = []
          parse input = let (Right (rem, _, evt)) = decodeEvent input
                         in evt : parse rem
          decodeEvent input = decodeOrFail input :: Either (LC8.ByteString, ByteOffset, String) (LC8.ByteString, ByteOffset, Event)

printEvents :: [Event] -> IO ()
printEvents events = mapM_ printEvent events
  where printEvent (ServerEvent (UnknownServerEvent txt)) = C8.putStrLn ("UnknownServerEvent: " <> txt <> "\ESC[0m")
        printEvent (ConsoleInput txt) = putStrLn ("ConsoleInput: " <> txt <> "\ESC[0m")
        printEvent event = print event

printQuestEvents :: [Event] -> IO ()
printQuestEvents events = printEvents $ filterQuestEvents events

filterQuestEvents :: [Event] -> [Event]
filterQuestEvents events =  L.filter questEvents events
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

filterTravelActions :: [Event] -> [Event]
filterTravelActions events = L.filter questEvents events
  where questEvents e = (not $ botCommand e) && (not $ simpleMoveCommands e) && (isLocationEvent e || isConsoleOutput e || isUnknownServerEvent e)
        isLocationEvent (ServerEvent (LocationEvent loc _)) = True
        isLocationEvent _ = False
        isConsoleOutput (ConsoleInput _) = True
        isConsoleOutput _ = False
        botCommand (ConsoleInput cmd) = T.isPrefixOf "/" cmd
        botCommand _ = False
        isUnknownServerEvent (ServerEvent (UnknownServerEvent _)) = True
        isUnknownServerEvent _ = False
        simpleMoveCommands e = e == (ConsoleInput "с")
                               || e == (ConsoleInput "в")
                               || e == (ConsoleInput "з")
                               || e == (ConsoleInput "ю")
                               || e == (ConsoleInput "вн")
                               || e == (ConsoleInput "вв")

obstacleActions :: [Event] -> Map (LocId, LocId) [Event]
obstacleActions questEvents = snd $ F.foldl' toActionMap ((Nothing, []), M.empty) questEvents
  where toActionMap ((Nothing, actions), travelActions) (ServerEvent (LocationEvent loc _)) = ((Just $ locId loc, []), travelActions)
        toActionMap acc@((Nothing, actions), travelActions) _ = acc
        toActionMap ((Just leftLocId, actions), travelActions) (ServerEvent (LocationEvent loc _)) = let newTravelActions = case actions of [] -> travelActions
                                                                                                                                            _ -> insert (leftLocId, locId loc) (L.reverse actions) travelActions
                                                                                                                                         in ((Just $ locId loc, []), newTravelActions)
        toActionMap ((leftLoc, actions), travelActions) evt = ((leftLoc, evt : actions), travelActions)
