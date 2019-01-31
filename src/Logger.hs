{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Logger ( runEvtLogger
              , runServerInputLogger
              , withLog
              , parseEvtLog
              , printEvents
              , printMove
              , filterQuestEvent
              , filterTravelActions
              , obstacleActions
              , scanDoorEvents
              , parseEventLogProducer
              ) where

import Protolude hiding ((<>), toStrict, Location)
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
import Control.Lens hiding ((&))

runEvtLogger evtBusInput h = runLogger evtBusInput h serverInteractions
runServerInputLogger evtBusInput h = runLogger evtBusInput h serverInput

runLogger :: Input Event -> Handle -> Pipe Event C8.ByteString IO () -> IO ()
runLogger evtBusInput h msgFilter = runEffect $ fromInput evtBusInput >-> msgFilter >-> PBS.toHandle h >> liftIO (C8.putStr "logger input stream ceased\n")

serverInteractions :: Pipe Event C8.ByteString IO ()
serverInteractions = forever $ await >>= \evt -> case evt of (SendToServer x) -> yield $ LC8.toStrict $ encode evt
                                                             (ServerEvent x) -> yield $ LC8.toStrict $ encode evt
                                                             _ -> return ()

serverInput :: Pipe Event C8.ByteString IO ()
serverInput = forever $ await >>= \evt -> case evt of (ServerInput input) -> yield input
                                                      _ -> return ()

withLog :: String -> ([Event] -> r) -> IO r
withLog filePath action = withFile filePath ReadMode $ \handle ->
  action <$> parseEvtLog . LC8.fromStrict <$> C8.hGetContents handle

parseEvtLog :: LC8.ByteString -> [Event]
parseEvtLog "" = []
parseEvtLog input = let (Right (rem, _, evt)) = decodeEvent input
                     in evt : parseEvtLog rem
  where decodeEvent input = decodeOrFail input :: Either (LC8.ByteString, ByteOffset, String) (LC8.ByteString, ByteOffset, Event)

parseEventLogProducer :: Monad m => LC8.ByteString -> Producer Event m ()
parseEventLogProducer input = parse input
  where parse "" = return ()
        parse bs = let (Right (rem, _, evt)) = decodeEvent bs
                    in do yield evt
                          parse rem
        decodeEvent input = decodeOrFail input :: Either (LC8.ByteString, ByteOffset, String) (LC8.ByteString, ByteOffset, Event)

printEvents :: [Event] -> IO ()
printEvents events = mapM_ printEvent events
  where printEvent (ServerEvent (UnknownServerEvent txt)) = C8.putStrLn ("UnknownServerEvent: " <> txt <> "\ESC[0m")
        printEvent (ServerEvent (MoveEvent txt)) = putStrLn ("MoveEvent: " <> txt <> "\ESC[0m")
        printEvent (ConsoleInput txt) = putStrLn ("ConsoleInput: " <> txt <> "\ESC[0m")
        printEvent (SendToServer txt) = putStrLn ("SendToServer: " <> txt <> "\ESC[0m")
        printEvent event = print event

printMove :: [LocToLocActions] -> IO ()
printMove moves = mapM_ printM moves
  where printM m = do print (fst m)
                      printEvents (snd m)

filterQuestEvent :: Event -> Bool
filterQuestEvent e = (not $ isServerInput e)
                        && (not $ isMoveEvent e)
                        && (e /= (ServerEvent PromptEvent))
                        && (not $ emptyUnknownServerEvent e)
                        && (not $ isConsoleOutput e)
  where emptyUnknownServerEvent (ServerEvent (UnknownServerEvent "")) = True
        emptyUnknownServerEvent _ = False
        isMoveEvent (ServerEvent (MoveEvent _)) = True
        isMoveEvent _ = False
        isServerInput (ServerInput _) = True
        isServerInput _ = False
        isConsoleOutput (ConsoleOutput _) = True
        isConsoleOutput _ = False

filterTravelActions :: Event -> Bool
filterTravelActions evt = isQuestEvent evt
  where isQuestEvent e = (not $ botCommand e) && (not $ simpleMoveCommands e) && (isLocationEvent e || isConsoleOutput e || isUnknownServerEvent e)
        isLocationEvent (ServerEvent (LocationEvent loc _ _)) = True
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

obstacleActions :: Monad m => Producer Event m () -> m (Map (LocationId, LocationId) [Event])
obstacleActions questEventsProducer = snd <$> PP.fold toActionMap ((Nothing, []), M.empty) identity questEventsProducer
  where toActionMap ((Nothing, actions), travelActions) (ServerEvent (LocationEvent loc _ _)) = ((Just $ loc^.locationId, []), travelActions)
        toActionMap acc@((Nothing, actions), travelActions) _ = acc
        toActionMap ((Just leftLocId, actions), travelActions) (ServerEvent (LocationEvent loc _ _)) =
          let newTravelActions = case actions of [] -> travelActions
                                                 _ -> insert (leftLocId, loc^.locationId) (L.reverse actions) travelActions
           in ((Just $ loc^.locationId, []), newTravelActions)
        toActionMap ((leftLoc, actions), travelActions) evt = ((leftLoc, evt : actions), travelActions)

type LocToLocActions = ([LocationId], [Event])
type LocPair = [LocationId]

scanDoorEvents :: [Event] -> [LocToLocActions]
scanDoorEvents evts = L.filter (\x -> (length $ fst x) >= 2) $ (\pair -> ((fst . snd) pair, (snd . fst) pair))  <$> (L.filter changeLocsOnly $ zipped)
  where doorEvents ([], actions) (ServerEvent (LocationEvent (Location locId _) _ _)) = ([locId], [])
        doorEvents (from:[], actions) (ServerEvent (LocationEvent (Location locId _) _ _)) = ([from, locId], [])
        doorEvents (from:to:[], actions) (ServerEvent (LocationEvent (Location locId _) _ _)) = ([to, locId], [])
        doorEvents (locPair, actions) ev = (locPair, ev:actions)
        events = scanl doorEvents ([], []) evts :: [LocToLocActions]
        zipped = events `zip` (L.tail events) :: [(LocToLocActions, LocToLocActions)]
        changeLocsOnly (left, right) = fst left /= fst right
