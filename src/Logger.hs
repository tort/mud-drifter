{-# LANGUAGE OverloadedStrings #-}

module Logger ( runLogger
              , withLog
              , printEvents
              , printQuestEvents
              , obstacleActions
              , filterQuestEvents
              , filterTravelActions
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
import Data.ByteString.Lazy hiding (foldl, foldl')
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as DBC8
import qualified Data.ByteString as BS
import qualified Data.Text.IO as DTIO
import Data.Monoid
import Data.Map.Strict hiding (foldl, foldl')
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Data.Text (isPrefixOf)
import qualified Data.Text as T

runLogger :: Input Event -> IO ()
runLogger evtBusInput = do async $ (bracketWithError
                                    (IO.openFile "evt.log" IO.WriteMode)
                                    (\e h -> do P.putStrLn $ P.show e
                                                IO.hClose h)
                                    (\h -> runEffect $ fromInput evtBusInput >-> PP.map (toStrict . encode) >-> PBS.toHandle h >> liftIO (IO.putStr "logger input stream ceased")))
                           return ()

withLog :: String -> ([Event] -> r) -> IO r
withLog filePath action = withFile filePath ReadMode $ \handle ->
  action <$> parse . BSL.fromStrict <$> BS.hGetContents handle
    where parse "" = []
          parse input = let (Right (rem, _, evt)) = decodeEvent input
                         in evt : parse rem
          decodeEvent input = decodeOrFail input :: Either (BSL.ByteString, ByteOffset, String) (BSL.ByteString, ByteOffset, E.Event)

printEvents :: [Event] -> IO ()
printEvents events = mapM_ printEvent events
  where printEvent (ServerEvent (UnknownServerEvent txt)) = DBC8.putStrLn ("UnknownServerEvent: " <> txt <> "\ESC[0m")
        printEvent (ConsoleInput txt) = DTIO.putStrLn ("ConsoleInput: " <> txt <> "\ESC[0m")
        printEvent event = IO.print event

printQuestEvents :: [Event] -> IO ()
printQuestEvents events = printEvents $ filterQuestEvents events

filterQuestEvents :: [Event] -> [Event]
filterQuestEvents events =  P.filter questEvents events
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
filterTravelActions events = P.filter questEvents events
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
                                                                                                                                            _ -> insert (leftLocId, locId loc) (Prelude.reverse actions) travelActions
                                                                                                                                         in ((Just $ locId loc, []), newTravelActions)
        toActionMap ((leftLoc, actions), travelActions) evt = ((leftLoc, evt : actions), travelActions)
