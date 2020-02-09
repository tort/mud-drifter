{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Person ( travel
              , travelTo
              , login
              , cover
              , initPerson
              , run
              , runE
              , Person(..)
              , MudServer(..)
              ) where

import Protolude hiding (Location)
import Pipes
import Mapper
import Event
import World
import Data.Maybe
import Control.Lens
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.List as L
import TextShow
import qualified Pipes.Concurrent as PC
import Pipes.Concurrent
import qualified Pipes.Prelude as PP
import Pipes.Network.TCP
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
import CommandExecutor
import Pipes.Lift
import RemoteConsole
import Logger

type Name = Text
type Password = Text

data MudServer = MudServer { host :: Text
                           , port :: Int
                           } deriving (Eq, Show)

data Person = Person { personName :: Name
                     , personPassword :: Password
                     , residence :: MudServer
                     } deriving (Eq, Show)

run :: MonadIO m => (Output ByteString, Input Event) -> Pipe Event Event m () -> m ()
run person task = runEffect $ fromInput (snd person) >-> task >-> commandExecutor >-> toOutput (fst person)

runE :: MonadIO m => (Output ByteString, Input Event) -> Pipe Event Event (ExceptT Text m) () -> m ()
runE person task = run person $ (runExceptP task) >>= print

initPerson :: Person -> IO (Output ByteString, Input Event)
initPerson person = do
  (outToServerBox, inToServerBox, sealToServerBox) <- spawn' $ newest 100
  toDrifterBox <- spawn $ newest 100
  (outToLoggerBox, inToLoggerBox, sealToLoggerBox) <- spawn' $ newest 100
  async $ connect mudHost mudPort $ \(sock, _) -> do
    toServerInputParserBox <- spawn $ newest 100
    print "connected"
    toRemoteConsoleBox <- spawn $ newest 100
    let commonOutput = (fst toRemoteConsoleBox) `mappend` (fst toServerInputParserBox) `mappend` outToLoggerBox
        emitPulseEvery = atomically $ PC.send (fst toDrifterBox) PulseEvent >> return ()
    async $ runRemoteConsole (outToServerBox, snd toRemoteConsoleBox)
    async $ runEffect $ fromInput (inToServerBox) >-> toSocket sock
    async $ runEffect $ parseServerEvents (fromInput (snd toServerInputParserBox)) >-> PP.map ServerEvent >-> toOutput (fst toDrifterBox) >>= liftIO . print
    async $ runServerInputLogger inToLoggerBox
    repeatedTimer emitPulseEvery (sDelay 1)
    runEffect $ fromSocket sock (2^15) >-> toOutput commonOutput >> (liftIO $ print "remote connection closed")
    performGC
    atomically sealToLoggerBox
    atomically sealToServerBox
    print "disconnected"
  return (outToServerBox, snd toDrifterBox)
    where mudHost = T.unpack . host . residence $ person
          mudPort = show . port . residence $ person

login :: Pipe Event Event IO ()
login = await >>= \case (ServerEvent CodepagePrompt) -> yield (SendToServer "5") >> login
                        (ServerEvent LoginPrompt) -> yield (SendToServer "генод") >> login
                        (ServerEvent PasswordPrompt) -> yield (SendToServer "каркасный") >> login
                        (ServerEvent WelcomePrompt) -> yield (SendToServer "")
                        _ -> login

findCurrentLoc :: MonadIO m => Pipe Event Event m ServerEvent
findCurrentLoc = yield (SendToServer "смотреть") >> go
  where go = await >>= \case evt@(ServerEvent locEvt@LocationEvent{}) -> yield evt >> return locEvt
                             evt -> yield evt >> go

travel :: MonadIO m => [LocationId] -> ServerEvent -> World -> Pipe Event Event (ExceptT Text m) ServerEvent
travel path locationEvent world = go path locationEvent
  where go [] _ = lift $ throwError "path lost"
        go [_] locEvt = return locEvt
        go remainingPath@(from:to:xs) locEvtFrom = (waitMove remainingPath >-> travelAction world locEvtFrom to) >>= \locEvt@LocationEvent{} ->
                                                                              go (dropWhile (/= (_locationId $ _location locEvt)) remainingPath) locEvt
        waitMove remainingPath = await >>= \case (ServerEvent l@LocationEvent{}) -> return l
                                                 evt -> yield evt >> waitMove remainingPath

travelTo :: MonadIO m => Text -> World -> Pipe Event Event (ExceptT Text m) ServerEvent
travelTo substr world = action findLocation
  where findLocation = findLocationsBy substr world
        action [] = lift $ throwError "no matching locations found"
        action [locTo] = (liftIO $ putStrLn ("travelling to " <> showt locTo)) >> travelAction locTo
        action _ = (liftIO $ printLocations substr world) >> (lift $ throwError  "multiple locations found")
        travelAction to = findCurrentLoc >>= \currLocEvt@(LocationEvent (Event.Location from _) _ _ _) ->
          case findTravelPath from to (_worldMap world)
            of (Just path) -> travelPath path currLocEvt
               Nothing -> lift $ throwError "no path found"
        travelPath path currLocEvt = travel path currLocEvt world

cover :: MonadIO m => World -> Pipe Event Event m ServerEvent
cover world = awaitFightBegin >> return PromptEvent
  where awaitFightBegin = await >>= \evt -> yield evt >> case evt of (ServerEvent FightPromptEvent) -> awaitFightEnd
                                                                     _ -> awaitFightBegin
        awaitFightEnd = await >>= \case (ServerEvent PromptEvent) -> awaitFightBegin
                                        PulseEvent -> awaitFightEnd
                                        evt -> yield evt >> awaitFightEnd
