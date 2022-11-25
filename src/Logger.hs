{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Logger ( runServerInputLogger
              , runBinaryLogger
              , serverLogDir
              , evtLogDir
              , printEvents
              , printEvent
              , obstacleActions
              , scanDoorEvents
              , parseEventLogProducer
              , archive
              , evtLogProducer
              , serverInteractions
              ) where

import Protolude hiding ((<>), toStrict, Location, yield, bracket)
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PBS
import Event
import qualified System.IO as IO
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
import Data.Time
import Data.Time.Format
import Pipes.Safe
import TextShow
import Pipes.Binary (DecodingError)
import qualified Pipes.Binary as PB
import Text.Shakespeare.Text

serverLogDir = archiveDir ++ "/server-input-log/"
evtLogDir = archiveDir ++ "/evt-log/"
archiveDir = "archive"

evtLogProducer :: FilePath -> Producer Event  IO (Either (DecodingError, Producer ByteString IO ()) ())
evtLogProducer file = producer ^. PB.decoded
  where
    producer =
      (lift $ IO.openFile file ReadMode) >>= \h ->
        PBS.fromHandle h >>
        (lift $ IO.hClose h)

runServerInputLogger :: Input ByteString -> IO ()
runServerInputLogger input = do
  startTimestamp <- timestamp
  withFile serverInputLogFilename WriteMode writeLog
  stopTimestamp <- timestamp
  archive serverInputLogFilename $ archivedLogFilename startTimestamp stopTimestamp
  where writeLog h = runEffect (fromInput input >-> PBS.toHandle h) >> C8.putStrLn "server log finished"
        archivedLogFilename startTs stopTs = serverLogDir ++ "genod-" ++ startTs ++ "__" ++ stopTs ++ ".log"
        timestamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" <$> getCurrentTime
        serverInputLogFilename = "server-input.log"

runBinaryLogger :: Input Event -> IO ()
runBinaryLogger input = do
  startTimestamp <- timestamp
  withFile eventLogFilename WriteMode writeLog
  stopTimestamp <- timestamp
  archive eventLogFilename $ archivedLogFilename startTimestamp stopTimestamp
  where
    writeLog h = runEffect $ fromInput input >-> serverInteractions >-> PBS.toHandle h
    archivedLogFilename startTs stopTs = evtLogDir ++ "evt-" ++ startTs ++ "__" ++ stopTs ++ ".log"
    timestamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" <$> getCurrentTime
    eventLogFilename = "evt.log"

archive :: String -> String -> IO ()
archive fromFileName toFilename =
  withFile fromFileName ReadMode $ \from ->
    withFile toFilename WriteMode $ \to -> do
      runEffect $ PBS.fromHandle from >-> PBS.toHandle to

serverInteractions :: Pipe Event C8.ByteString IO ()
serverInteractions =
  forever $
  await >>= \evt ->
    case evt of
      (SendToServer x) -> yield $ LC8.toStrict $ encode evt
      (ConsoleInput x) -> yield $ LC8.toStrict $ encode evt
      (ServerEvent x) -> yield $ LC8.toStrict $ encode evt
      _ -> return ()

serverInput :: Pipe Event ByteString IO ()
serverInput = forever $ await >>= \evt -> case evt of (ServerInput input) -> yield input
                                                      _ -> return ()

parseEventLogProducer :: Monad m => LC8.ByteString -> Producer Event m ()
parseEventLogProducer input = parse input
  where parse "" = return ()
        parse bs = case decodeEvent bs of (Right (rem, _, evt)) -> yield evt >> parse rem
                                          (Left (_, _, err)) -> yield $ ConsoleOutput $ "error: " <> C8.pack err <> "\n"
        decodeEvent input = decodeOrFail input :: Either (LC8.ByteString, ByteOffset, String) (LC8.ByteString, ByteOffset, Event)

printEvents :: Consumer Event IO a
printEvents = forever $ await >>= lift . printEvent

printEvent (ServerEvent (UnknownServerEvent txt)) = C8.putStrLn ("UnknownServerEvent: " <> txt <> "\ESC[0m")
printEvent (ServerEvent (MoveEvent txt)) = putStrLn ("MoveEvent: " <> txt <> "\ESC[0m")
printEvent (ConsoleInput txt) = putStrLn ("ConsoleInput: " <> txt <> "\ESC[0m")
printEvent (SendToServer txt) = putStrLn ("SendToServer: " <> txt <> "\ESC[0m")
printEvent (ServerEvent (LocationEvent (Location id title) _ _ _)) = putStrLn ("LocationEvent: [" <> (showt id) <> "]\ESC[0m")
printEvent (ServerEvent (TakeFromContainer item container)) = putStrLn [st|Вы взяли #{showt item} из #{showt container}|]
printEvent event = printT . T.pack . show $ event

{-printMove :: [LocToLocActions] -> IO ()
printMove moves = mapM_ printM moves
  where printM m = do print (fst m)
                      printEvents (snd m)-}

obstacleActions :: Monad m => Producer Event m () -> m (Map (LocationId, LocationId) [Event])
obstacleActions questEventsProducer = snd <$> PP.fold toActionMap ((Nothing, []), M.empty) identity questEventsProducer
  where toActionMap ((Nothing, actions), travelActions) (ServerEvent (LocationEvent loc _ _ _)) = ((Just $ loc^.locationId, []), travelActions)
        toActionMap acc@((Nothing, actions), travelActions) _ = acc
        toActionMap ((Just leftLocId, actions), travelActions) (ServerEvent (LocationEvent loc _ _ _)) =
          let newTravelActions = case actions of [] -> travelActions
                                                 _ -> insert (leftLocId, loc^.locationId) (L.reverse actions) travelActions
           in ((Just $ loc^.locationId, []), newTravelActions)
        toActionMap ((leftLoc, actions), travelActions) evt = ((leftLoc, evt : actions), travelActions)

type LocToLocActions = ([LocationId], [Event])
type LocPair = [LocationId]

scanDoorEvents :: [Event] -> [LocToLocActions]
scanDoorEvents evts = L.filter (\x -> (length $ fst x) >= 2) $ (\pair -> ((fst . snd) pair, (snd . fst) pair))  <$> (L.filter changeLocsOnly $ zipped)
  where doorEvents ([], actions) (ServerEvent (LocationEvent (Location locId _) _ _ _)) = ([locId], [])
        doorEvents (from:[], actions) (ServerEvent (LocationEvent (Location locId _) _ _ _)) = ([from, locId], [])
        doorEvents (from:to:[], actions) (ServerEvent (LocationEvent (Location locId _) _ _ _)) = ([to, locId], [])
        doorEvents (locPair, actions) ev = (locPair, ev:actions)
        events = scanl doorEvents ([], []) evts :: [LocToLocActions]
        zipped = events `zip` (L.tail events) :: [(LocToLocActions, LocToLocActions)]
        changeLocsOnly (left, right) = fst left /= fst right
