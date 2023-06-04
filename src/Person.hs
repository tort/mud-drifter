{-# LANGUAGE TypeApplications #-}

module Person where

import Protolude hiding (yield, Location, Down, Up, Left, Right, Dual, to)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
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
import TextShow.Generic
import qualified Pipes.Concurrent as PC
import Pipes.Concurrent
import qualified Pipes.Prelude as PP
import Pipes.Network.TCP
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
import Control.Monad
import CommandExecutor
import Pipes.Lift
import RemoteConsole
import Logger
import System.IO hiding (getLine, print, putStr, putStrLn)

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
          (void (liftIO . atomically . PC.send outChan $ evt))
        evt@SendOnPulse {} ->
          (void (liftIO . atomically . PC.send outChan $ evt))
        _ -> pure ()

runSubtask :: (MonadIO m, Show a) => (Output Event, Output Event, Input Event) -> Pipe Event Event m a -> m ()
runSubtask (outChan, outPulse, inChan) task =
  runEffect $ fromInput inChan >-> (task >>= print) >-> sendOutput
  where
    sendOutput =
      forever $
      await >>= \case
        PulseEvent -> 
          (void (liftIO . atomically . PC.send outPulse $ PulseEvent))
        evt@SendToServer {} ->
          (void (liftIO . atomically . PC.send outChan $ evt))
        evt@SendOnPulse {} ->
          (void (liftIO . atomically . PC.send outChan $ evt))
        _ -> pure ()

runE :: (MonadIO m, Show a) => (Output Event, Input Event) -> Pipe Event Event (ExceptT Text m) a -> m ()
runE person task = run person (runExceptP task)

initPerson :: Person -> IO (Output Event, Input Event)
initPerson person = do
  (outToServerBox, inToServerBox, sealToServerBox) <- spawn' $ newest 7
  (outToExecutorBox, inToExecutorBox, sealToExecutorBox) <- spawn' $ newest @Event 7
  toDrifterBox <- spawn $ newest 100
  (outToLoggerBox, inToLoggerBox, sealToLoggerBox) <- spawn' $ newest 100
  (outToBinaryLoggerBox, inToBinaryLoggerBox, sealToBinaryLoggerBox) <- spawn' $ newest 100
  async $ connect mudHost mudPort $ \(sock, _) -> do
    (outToServerInputParserBox, inToServerInputParserBox, sealToServerInputParserBox) <- spawn' $ newest 100
    print "connected"
    (outToRemoteConsoleBox, inToRemoteConsoleBox, sealToRemoteConsoleBox) <- spawn' $ newest 100
    let commonOutput = outToRemoteConsoleBox `mappend` outToServerInputParserBox `mappend` outToLoggerBox
    async $ runRemoteConsole (outToExecutorBox <> outToBinaryLoggerBox, inToRemoteConsoleBox)
    async $ runEffect $ fromInput inToServerBox >-> toSocket sock
    async $ runEffect $ fromInput inToExecutorBox >-> commandExecutor >-> toOutput outToServerBox >> liftIO (print "executor pipe finished")
    async $ runEffect $ parseServerEvents (fromInput inToServerInputParserBox) >-> PP.map ServerEvent >-> toOutput (fst toDrifterBox <> outToBinaryLoggerBox) *> lift (print "server parser finished")
    async $ runServerInputLogger inToLoggerBox
    async $ runBinaryLogger inToBinaryLoggerBox
    runEffect $ fromSocket sock (2^15) >-> toOutput commonOutput >> liftIO (print "remote connection closed") >> liftIO (atomically sealToBinaryLoggerBox)
    performGC
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

checkCases mob = Pipes.yield (SendToServer "смотр") *> (mapM_ (ddo) . fmap (<> " " <> mob) $ ["благословить", "думать", "бояться", "бухать", "хваст", "указать"])
identifyNameCases :: Set (ObjRef Mob InRoomDesc) ->  Map Text Text -> Pipe Event Event IO ()
identifyNameCases knownMobs aliases =
  await >>= \case
    (ServerEvent l@(LocationEvent _ _ [mob] _ _)) ->
      --(lift . genericPrintT) l *>
      case (isKnown mob, getAlias mob) of
        (False, Just alias) ->
          (checkCases alias) *> identifyNameCases (S.insert mob knownMobs) aliases
        _ -> identifyNameCases knownMobs aliases
    PulseEvent -> yield PulseEvent *> identifyNameCases knownMobs aliases
    _ -> identifyNameCases knownMobs aliases
  where
    isKnown mob = S.member mob knownMobs
    getAlias mob = M.lookup (unObjRef mob) $ aliases



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

type ZoneId = Text

mapNominatives :: MonadIO m => Map (ObjRef Mob InRoomDesc) MobStats -> Map Int ZonedLocation -> Pipe Event Event m ()
mapNominatives knownMobs locations = PP.map mapEvt
  where
    knownNominatives ::
         Map (ObjRef Mob Nominative, Text) (ObjRef Mob InRoomDesc)
    knownNominatives =
      M.fromList . (fmap) fromJust .
      (fmap)
        (\s -> Just ((s ^. nameCases . nominative, s ^. zone), s ^. nameCases . inRoomDesc)
           ) .
      M.elems $
      knownMobs
    mapEvt evt@(ServerEvent (LocationEvent l@(Location locId title) objects mobs exits zone)) =
      ServerEvent $
      LocationEvent l objects (fmap (updateNom (M.lookup locId locations)) mobs) exits zone
    mapEvt evt = evt
    updateNom :: Maybe ZonedLocation -> ObjRef Mob InRoomDesc -> ObjRef Mob InRoomDesc
    updateNom Nothing mob = mob
    updateNom (Just (ZonedLocation _ zone)) mob =
      fromMaybe mob . (fmap ObjRef . toInRoomDesc zone) . extractNominative .
      T.toLower .
      unObjRef $
      mob
    extractNominative :: Text -> Maybe Text
    extractNominative mob
      | T.isSuffixOf " сражается с ВАМИ!" mob =
        T.stripSuffix " сражается с ВАМИ!" $ mob
      | T.isSuffixOf " отдыхает здесь." mob =
        T.stripSuffix " отдыхает здесь." $ mob
      | otherwise = Just mob
    toInRoomDesc :: ZoneId -> Maybe Text -> Maybe Text
    toInRoomDesc zone mob =
      mob >>= \m ->
        M.lookup (ObjRef m, zone) knownNominatives >>= pure . unObjRef
      --fromMaybe mob $ (unObjRef <$> (M.lookup (ObjRef mob, zone) knownNominatives))

killEmAll :: World -> Pipe Event Event IO ()
killEmAll world = forever lootAll >-> awaitTargets [] False 0
  where
    lootAll =
      await >>= \evt -> do
        yield evt
        case evt of
          evt@(ServerEvent ExpUpEvent) -> do
            yield (SendToServer "взять труп")
            yield (SendToServer "взять все труп")
            yield (SendToServer "брос труп")
          evt@(ServerEvent (LootItem _ _)) -> do
            yield (SendToServer "полож все мешок")
          evt -> pure ()
    awaitTargets mobs inFight killAttempts =
      await >>= \case
        ServerEvent e@CantSeeTarget ->
          yield (SendToServer "смотр") *> awaitTargets mobs False killAttempts
        evt@(ServerEvent (MobWentOut mobNom)) -> do
          let newMobs =
                case pure . _inRoomDesc . _nameCases =<<
                     (M.!?) (_nominativeToMob world) mobNom of
                  Nothing -> mobs
                  Just deadMob -> L.delete deadMob mobs
           in awaitTargets newMobs inFight killAttempts
        evt@(ServerEvent (MobWentIn mobNom)) -> do
          let newMobs =
                case pure . _inRoomDesc . _nameCases =<<
                     (M.!?) (_nominativeToMob world) mobNom of
                  Nothing -> mobs
                  Just deadMob -> L.insert deadMob mobs
           in awaitTargets newMobs inFight killAttempts
        evt@(ServerEvent e@(MobRipEvent mr)) -> do
          let newMobs =
                case pure . _inRoomDesc . _nameCases =<<
                     (M.!?) (_nominativeToMob world) mr of
                  Nothing -> mobs
                  Just deadMob -> L.delete deadMob mobs
           in awaitTargets newMobs False killAttempts
        evt@(ServerEvent e@FightPromptEvent {}) -> awaitTargets mobs True 0
        evt@(ServerEvent e@PromptEvent {}) ->
          awaitTargets mobs False killAttempts
        evt@(ServerEvent (LocationEvent _ _ [] _ _)) ->
          awaitTargets [] False killAttempts
        evt@(ServerEvent (LocationEvent _ _ lmobs _ zone)) ->
          awaitTargets lmobs inFight killAttempts
        PulseEvent ->
          case (chooseTarget mobs, inFight) of
            (Just mobAlias, False) -> do
              if killAttempts < 3
                then yield (SendToServer $ "убить " <> (unObjRef $ mobAlias)) *>
                     awaitTargets mobs True (killAttempts + 1)
                else ddo "смотр" *> awaitTargets mobs False killAttempts
            (_, True) -> awaitTargets mobs inFight 0
            _ -> yield PulseEvent *> awaitTargets mobs inFight 0
        evt -> awaitTargets mobs inFight killAttempts
    findAlias :: ObjRef Mob InRoomDesc -> Maybe (ObjRef Mob Alias, Bool)
    findAlias mobRef =
      liftA2 (,) (_alias . _nameCases) (_everAttacked) <$>
      M.lookup mobRef (_inRoomDescToMob world)
    chooseTarget :: [ObjRef Mob InRoomDesc] -> Maybe (ObjRef Mob Alias)
    chooseTarget =
      preview (traversed . to findAlias . traversed . filtered (view _2) . _1)

travelToMob :: MonadIO m => World -> ObjRef Mob InRoomDesc -> Pipe Event Event (ExceptT Text m) (Int)
travelToMob world mobNom = pipe mobArea
  where mobArea :: Maybe (Map (Int) Int)
        mobArea = (_inRoomDescToMobOnMap world M.!?) =<< pure mobNom
        pipe (Just area)
          | M.size area < 1 = lift $ throwError "no habitation found"
          | M.size area > 1 = lift $ throwError "multiple habitation locations found"
          | M.size area == 1 = travelToLoc loc world >> pure (L.head . M.keys $ area)
        pipe Nothing = lift $ throwError "no habitation found"
        loc = showt . L.head . M.keys . fromJust $ mobArea

{-
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
        trackHp maxHp maxMv = await >>= \evt -> yield evt >> case evt of (ServerEvent (PromptEvent hp mv)) -> if div (100 * hp) maxHp < 90 || div (100 * mv) maxMv < 20
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
runZone :: MonadIO m => World -> Int -> Pipe Event Event (ExceptT Text m) ()
runZone world startFrom = travelToLoc startFrom world *>
  cover world >-> supplyTask >-> killEmAll world >-> travel path world
  where path = zonePath world startFrom
-}

printBuffered :: IO () -> IO ()
printBuffered io =
  hGetBuffering stdout >>= \bmode ->
    hSetBuffering stdout LineBuffering *> io *> hSetBuffering stdout bmode

runTwo :: (Output Event, Input Event) -> Pipe Event Event IO () -> Pipe Event Event IO () -> IO ()
runTwo person@(personOut, personIn) task1 task2 =
  spawn' (newest 100) >>= \(subtaskOut1, subtaskIn1, seal1) ->
    spawn' (newest 100) >>= \(subtaskOut2, subtaskIn2, seal2) ->
      repeatedTimer (emitPulseEvery subtaskOut1) (sDelay 1) >>= \timer ->
        (async $
         runSubtask (personOut, subtaskOut2, subtaskIn1) task1 >>
         (atomically seal1) *> (atomically seal2) *> printBuffered (print "subtask1 finished")) *>
        (async $
         run (personOut, subtaskIn2) task2 >>
         (atomically seal1) *> (atomically seal2) *> printBuffered (print "subtask2 finished")) *>
        (runEffect $
         fromInput personIn >-> toOutput (subtaskOut1 <> subtaskOut2) >>
         print "read finished") *>
        stopTimer timer *>
        pure ()
  where
    emitPulseEvery out = atomically $ void (PC.send out PulseEvent)

runThree :: (Output Event, Input Event) -> Pipe Event Event IO () -> Pipe Event Event IO () -> Pipe Event Event IO () -> IO ()
runThree person@(personOut, personIn) task1 task2 task3 =
  spawn' (newest 10) >>= \(subtaskOut1, subtaskIn1, seal1) ->
    spawn' (newest 10) >>= \(subtaskOut2, subtaskIn2, seal2) ->
      spawn' (newest 10) >>= \(subtaskOut3, subtaskIn3, seal3) ->
        repeatedTimer (emitPulseEvery subtaskOut1) (sDelay 1) >>= \timer ->
          (async $
          runSubtask (personOut, subtaskOut2, subtaskIn1) task1 >>
          (atomically seal1) *> (atomically seal2) *> (atomically seal3) *> printBuffered (print "subtask1 finished")) *>
          (async $
          runSubtask (personOut, subtaskOut3, subtaskIn2) task2 >>
          (atomically seal1) *> (atomically seal2) *> (atomically seal3) *> printBuffered (print "subtask2 finished")) *>
          (async $
          run (personOut, subtaskIn3) task3 >>
          (atomically seal1) *> (atomically seal2) *> (atomically seal3) *> printBuffered (print "subtask3 finished")) *>
          (runEffect $
          fromInput personIn >-> toOutput (subtaskOut1 <> subtaskOut2 <> subtaskOut3) >>
          print "read finished") *>
          stopTimer timer *>
          pure ()
  where
    emitPulseEvery out = atomically $ void (PC.send out PulseEvent)


waitMsg msg = await >>= \case
  ServerEvent (UnknownServerEvent txt) -> if C8.isInfixOf msg txt then pure () else waitMsg txt
  _ -> waitMsg msg


travelAction :: MonadIO m => World -> Int -> Int -> Maybe (Pipe Event Event m ())
travelAction world from to =
  customTransition <|> transition 
  where
    customTransition :: MonadIO m => Maybe (Pipe Event Event m ())
    customTransition = travelActions ^. at (from, to)
    transition :: MonadIO m => Maybe (Pipe Event Event m ())
    transition = ddo . genericShowt <$> (_directions world) ^. at (from, to)

travel :: MonadIO m => [Int] -> World -> Pipe Event Event (ExceptT Text m) ()
travel path world = waitMove path False
  where
    waitMove [] _ = lift $ throwError "path lost"
    waitMove [_] _ = pure ()
    waitMove remainingPath@(from:to:xs) inAction =
      await >>= 
      \case 
        (ServerEvent newLoc@LocationEvent {}) ->
          waitMove
            (dropWhile (/= (_locationId $ _location newLoc)) remainingPath)
            False
        PulseEvent ->
          if not inAction
            then
              case travelAction world from to of Nothing -> lift $ throwError "unknown transition"
                                                 Just p -> p >> waitMove remainingPath True
            else yield PulseEvent *> waitMove remainingPath inAction
        _ -> waitMove remainingPath inAction

travelToLoc :: MonadIO m => Text -> World -> Pipe Event Event (ExceptT Text m) ()
travelToLoc substr world = action findLocation
  where
    findLocation = findLocationsBy substr world
    action [] = lift $ throwError "no matching locations found"
    action [locTo] =
      liftIO (putStrLn ("travelling to " <> showt locTo)) >>
      travelAction locTo
    action _ =
      lift (throwError "multiple locations found")
    travelAction to =
      findCurrentLoc >>= \currLocEvt@(LocationEvent (Event.Location from _) _ _ _ _) ->
        case findTravelPath from to (_worldMap world) of
          (Just path) -> travel path world
          Nothing -> lift $ throwError "no path found"

findCurrentLoc :: MonadIO m => Pipe Event Event m ServerEvent
findCurrentLoc = yield (SendToServer "смотреть") >> go
  where go = await >>= \case evt@(ServerEvent locEvt@LocationEvent{}) -> yield evt >> return locEvt
                             evt -> yield evt >> go
