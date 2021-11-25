{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

module Person ( travel
              , travelToLoc
              --, travelToMob
              --, travelAndKill
              , findCurrentLoc
              , login
              , cover
              , initPerson
              , run
              , runE
              , killEmAll
              --, runZone
              , Person(..)
              , MudServer(..)
              ) where

import Protolude hiding (Location, yield)
import Pipes
import Mapper
import Event
import World
import Data.Maybe
import Control.Lens
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map.Strict as M
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

run :: (MonadIO m, Show a) => (Output Event, Input Event) -> Pipe Event Event m a -> m ()
run (outChan, inChan) task =
  runEffect $ fromInput inChan >-> (task >>= print) >-> sendOutput
  where
    sendOutput =
      forever $
      await >>= \case
        evt@SendToServer {} ->
          (liftIO . atomically . PC.send outChan $ evt) >> pure ()
        evt@SendOnPulse {} ->
          (liftIO . atomically . PC.send outChan $ evt) >> pure ()
        _ -> pure ()

runE :: (MonadIO m, Show a) => (Output Event, Input Event) -> Pipe Event Event (ExceptT Text m) a -> m ()
runE person task = run person $ (runExceptP task)

initPerson :: Person -> IO (Output Event, Input Event)
initPerson person = do
  (outToServerBox, inToServerBox, sealToServerBox) <- spawn' $ newest 5
  (outToExecutorBox, inToExecutorBox, sealToExecutorBox) <- spawn' $ newest 5
  toDrifterBox <- spawn $ newest 100
  (outToLoggerBox, inToLoggerBox, sealToLoggerBox) <- spawn' $ newest 100
  async $ connect mudHost mudPort $ \(sock, _) -> do
    toServerInputParserBox <- spawn $ newest 100
    print "connected"
    toRemoteConsoleBox <- spawn $ newest 100
    let commonOutput = (fst toRemoteConsoleBox) `mappend` (fst toServerInputParserBox) `mappend` outToLoggerBox
        emitPulseEvery = atomically $ PC.send outToExecutorBox PulseEvent >> return ()
    async $ runRemoteConsole (outToServerBox, snd toRemoteConsoleBox)
    async $ runEffect $ fromInput (inToServerBox) >-> toSocket sock
    async $ runEffect $ fromInput (inToExecutorBox) >-> commandExecutor >-> toOutput outToServerBox
    async $ runEffect $ parseServerEvents (fromInput (snd toServerInputParserBox)) >-> PP.map ServerEvent >-> toOutput (fst toDrifterBox) >>= liftIO . print
    async $ runServerInputLogger inToLoggerBox
    repeatedTimer emitPulseEvery (sDelay 1)
    runEffect $ fromSocket sock (2^15) >-> toOutput commonOutput >> (liftIO $ print "remote connection closed")
    performGC
    atomically sealToLoggerBox
    atomically sealToServerBox
    atomically sealToExecutorBox
    print "disconnected"
  return (outToExecutorBox, snd toDrifterBox)
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

travel :: MonadIO m => [LocationId] -> ServerEvent -> World -> Pipe Event Event (ExceptT Text m) ()
travel path locationEvent world = doStep path
  where
    doStep [] = lift $ throwError "path lost"
    doStep [_] = pure ()
    doStep p@(from:to:xs) = travelAction world from to >> waitMove p
    waitMove remainingPath =
      await >>= \case
        (ServerEvent newLoc@LocationEvent {}) ->
           doStep $ dropWhile (/= (_locationId $ _location newLoc)) remainingPath
        _ -> waitMove remainingPath

travelToLoc :: MonadIO m => Text -> World -> Pipe Event Event (ExceptT Text m) ()
travelToLoc substr world = action findLocation
  where
    findLocation = findLocationsBy substr world
    action [] = lift $ throwError "no matching locations found"
    action [locTo] =
      (liftIO $ putStrLn ("travelling to " <> showt locTo)) >>
      travelAction locTo
    action _ =
      (liftIO $ printLocations substr world) >>
      (lift $ throwError "multiple locations found")
    travelAction to =
      findCurrentLoc >>= \currLocEvt@(LocationEvent (Event.Location from _) _ _ _) ->
        case findTravelPath from to (_worldMap world) of
          (Just path) -> doStep path currLocEvt
          Nothing -> lift $ throwError "no path found"
    doStep path currLocEvt = travel path currLocEvt world

cover :: MonadIO m => World -> Pipe Event Event m ServerEvent
cover world = trackBash >-> awaitFightBegin
  where awaitFightBegin = await >>= \evt -> yield evt >> case evt of (ServerEvent FightPromptEvent{}) -> awaitFightEnd
                                                                     _ -> awaitFightBegin
        awaitFightEnd = await >>= \case evt@(ServerEvent PromptEvent{}) -> yield evt >> awaitFightBegin
                                        PulseEvent -> awaitFightEnd
                                        evt -> yield evt >> awaitFightEnd

trackBash :: Monad m => Pipe Event Event m ServerEvent
trackBash = forever awaitBash
  where awaitBash = await >>= \case evt@(ServerEvent ImBashedEvent) -> yield evt >> stand
                                    evt -> yield evt
        stand = await >>= \case PulseEvent -> yield (SendToServer "встать")
                                evt -> yield evt >> stand

killEmAll :: MonadIO m => World -> Pipe Event Event m ServerEvent
killEmAll world = (forever lootAll) >-> awaitTargets
  where lootAll = await >>= \case evt@(ServerEvent MobRipEvent) -> do yield (SendToServer "взять труп")
                                                                      yield (SendToServer "взять все труп")
                                                                      yield (SendToServer "брос труп")
                                                                      yield evt
                                  evt@(ServerEvent (LootItem _ _)) -> do yield (SendToServer "полож все мешок")
                                                                         yield evt
                                  evt -> yield evt
        awaitTargets = await >>= \case evt@(ServerEvent (LocationEvent _ _ [] _)) -> yield evt >> awaitTargets
                                       evt@(ServerEvent (LocationEvent _ _ mobs _)) -> attack evt (chooseTarget mobs)
                                       evt -> yield evt >> awaitTargets
        attack evt Nothing = yield evt >> awaitTargets
        attack e (Just mobAlias) = await >>= \case PulseEvent -> yield (SendToServer $ "убить " <> (unObjRef mobAlias)) >> awaitFightBegin 0
                                                   evt -> yield evt >> attack e (Just mobAlias)
        awaitFightBegin pulsesCount = await >>= \case evt@(ServerEvent FightPromptEvent{}) -> yield evt >> awaitFightEnd
                                                      PulseEvent -> if pulsesCount < 2
                                                                       then awaitFightBegin (pulsesCount + 1)
                                                                       else yield (SendToServer "смотреть") >> awaitTargets
                                                      evt -> yield evt >> awaitFightBegin pulsesCount
        awaitFightEnd = await >>= \case evt@(ServerEvent PromptEvent{}) -> yield evt >> watch
                                        PulseEvent -> awaitFightEnd
                                        evt -> yield evt >> awaitFightEnd
        watch = await >>= \case PulseEvent -> yield (SendToServer "смотреть") >> awaitTargets
                                evt -> yield evt >> watch
        findAlias :: MobRoomDesc -> Maybe (ObjRef Mob Alias)
        findAlias mobRef = _alias . _nameCases =<< M.lookup mobRef (_inRoomDescToMob world)
        chooseTarget :: [ObjRef Mob InRoomDesc] -> Maybe (ObjRef Mob Alias)
        chooseTarget targets = join . find isJust . fmap findAlias $ targets

{-
travelToMob :: MonadIO m => World -> MobRoomDesc -> Pipe Event Event (ExceptT Text m) ServerEvent
travelToMob world mob = pipe $ mobArea
  where mobArea :: Maybe (Map LocationId Int)
        mobArea = M.lookup mob . _mobsDiscovered $ world
        pipe (Just area)
          | M.size area < 1 = lift $ throwError "no habitation found"
          | M.size area > 1 = lift $ throwError "multiple habitation locations found"
          | M.size area == 1 = travelToLoc loc world
        pipe Nothing = lift $ throwError "no habitation found"
        loc = showt . L.head . M.keys . fromJust $ mobArea

travelAndKill :: MonadIO m => World -> Map MobRoomDesc Text -> MobRoomDesc -> Pipe Event Event (ExceptT Text m) ServerEvent
travelAndKill world targets mob = pipe $ mobArea
  where mobArea :: Maybe (Map LocationId Int)
        mobArea = M.lookup mob . _mobsDiscovered $ world
        pipe (Just area)
          | M.size area < 1 = lift $ throwError "no habitation found"
          | M.size area > 1 = lift $ throwError "multiple habitation locations found"
          | M.size area == 1 = travelToLoc loc world >>= tryFight
        pipe Nothing = lift $ throwError "no habitation found"
        loc = showt . L.head . M.keys . fromJust $ mobArea
        tryFight locEvt = if L.elem mob (_mobs locEvt)
                             then fight
                             else lift $ throwError "target is absent"
        fight = await >>= \case PulseEvent -> yield (SendToServer $ "убить " <> target [mob]) >> awaitFightEnd
                                evt -> yield evt >> fight
        awaitFightEnd = await >>= \case evt@(ServerEvent p@PromptEvent{}) -> return p
                                        PulseEvent -> awaitFightEnd
                                        evt -> yield evt >> awaitFightEnd
        target = fromJust . join . find isJust . fmap (flip M.lookup targets)
-}

supplyTask :: Monad m => Pipe Event Event m ServerEvent
supplyTask = init
  where init = await >>= \case PulseEvent -> yield (SendToServer "счет все") >> readStats
                               evt -> yield evt >> init
        readStats = await >>= \case evt@(ServerEvent (MyStats maxHp maxMv)) -> yield evt >> trackHp maxHp maxMv
                                    evt -> yield evt >> readStats
        trackHp maxHp maxMv = await >>= \evt -> yield evt >> case evt of (ServerEvent (PromptEvent hp mv)) -> if (div (100 * hp) maxHp) < 90 || (div (100 * mv) maxMv) < 20
                                                                                                                 then rest maxHp maxMv
                                                                                                                 else trackHp maxHp maxMv
                                                                         _ -> trackHp maxHp maxMv
        rest maxHp maxMv = await >>= \case PulseEvent -> yield (SendToServer "отдохнуть") >> awaitFullHp maxHp maxMv
                                           evt -> yield evt >> rest maxHp maxMv
        awaitFullHp maxHp maxMv = await >>= \case evt@(ServerEvent (PromptEvent hp mv)) -> if hp >= maxHp && mv >= maxMv
                                                                                              then yield (SendToServer "встать") >> yield evt >> trackHp maxHp maxMv
                                                                                              else yield evt >> awaitFullHp maxHp maxMv
                                                  PulseEvent -> awaitFullHp maxHp maxMv
                                                  evt -> yield evt >> awaitFullHp maxHp maxMv

{-
runZone :: MonadIO m => World -> Map MobRoomDesc MobStats -> Text -> Int -> Pipe Event Event (ExceptT Text m) ServerEvent
runZone world allKillableMobs goTo startFrom = travelToLoc goTo world >>= \locEvt ->
  cover world >-> supplyTask >-> killEmAll world >-> travel path locEvt world
  where path = zonePath world startFrom
-}
