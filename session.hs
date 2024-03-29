
world <- loadWorld "/Users/tort/workspace/mud-drifter/" M.empty

:{
genod = Person { personName = "генод"
               , personPassword = "каркасный"
               , residence = MudServer "bylins.su" 4000
               }
:}

g <- initPerson genod
g & run $ login

-- run quest
loadWorld >>= \world -> loadCachedLocations >>= \locs -> loadCachedMobAliases >>= \aliases -> loadCachedMobData >>= \knownMobs -> runThree g (identifyNameCases (S.fromList . M.keys $ knownMobs) aliases) (mapNominatives knownMobs locs >-> killEmAll world >> pure ()) (fmap (fromRight ()) . runExceptP $ Quest.oldBoots world *> Quest.whiteSpider world *> Quest.fisherman world *> Quest.well world *>  travelToLoc "5000" world *> pure ())

-- roam
loadCachedMobAliases >>= \aliases -> loadCachedMobData >>= \knownMobs -> runTwo g (scanZoneEvent >-> PP.map (fromJust . fst) >-> mapNominatives knownMobs >-> killEmAll world >> pure ()) (identifyNameCases (S.fromList . M.keys $ knownMobs) aliases)

-- explore
loadCachedMobAliases >>= \aliases -> loadCachedMobData >>= \knownMobs -> (g & run $ (identifyNameCases (S.fromList . M.keys $ knownMobs) aliases))

g & run $ yield (SendToServer "постой") >> yield (SendToServer "0")

findTravelPath 6049 5000 <$> liftA2 buildMap loadCachedLocations loadCachedDirections

-- travel covered
loadCachedLocations >>= \locs -> loadCachedMobAliases >>= \aliases -> loadCachedMobData >>= \knownMobs -> runThree g (identifyNameCases (S.fromList . M.keys $ knownMobs) aliases) (mapNominatives knownMobs locs >-> killEmAll world >> pure ()) (fmap (fromRight ()) . runExceptP $ travelToLoc "5000" world *> pure ())

 
rent = ddo "постой" *> ddo "0"
 
loadCachedLocations >>= \locs -> loadCachedMobAliases >>= \aliases -> loadCachedMobData >>= \knownMobs -> runTwo g (mapNominatives knownMobs locs >-> killEmAll world >> pure ()) (fmap (fromRight ()) . runExceptP $ wellQuest *> pure ())

loadCachedLocations >>= \locs -> loadCachedMobAliases >>= \aliases -> loadCachedMobData >>= \knownMobs -> runTwo g (mapNominatives knownMobs locs >-> killEmAll world >> pure ()) (fmap (fromRight ()) . runExceptP $ travelToMob world (ObjRef "агрессивная плесень") *> pure ())

runE g $ travelToMob world (ObjRef "Рогатый жук воинственно водит усами здесь.")

loadCachedMobAliases >>= \aliases -> loadCachedMobData >>= \knownMobs -> run g $ (identifyNameCases (S.fromList . M.keys $ knownMobs) aliases)

:{
 putStrLn . (L.!! 0) . fromJust . snd =<< (\(l, r) -> (l, ) <$> r) =<<
  pure .
  fmap Control.Monad.sequence .
  fmap (preview (_Left . _2 . Control.Lens.to PP.toListM)) =<<
  PP.toListM'
    (PA.parsed
    (
      (string . encodeUtf8) "Вы посмотрели по сторонам." *> C.endOfLine *>
      cs *> string "0;33m"
    )
       (loadServerEvents "test/logs/glanceAround.1.log"))
:}

:{
 (\(l, r) -> (l, ) <$> r) =<<
  pure .
  fmap Control.Monad.sequence .
  fmap (preview (_Left . _2 . Control.Lens.to PP.toListM)) =<<
  PP.toListM'
    (PA.parsed
    (hitEventGenitive) 
       (loadServerEvents "test/logs/glanceAround.1.log"))
:}

runEffect (PP.mapM_ (pprint) <-< PA.parsed (serverInputParser) (loadServerEvents "test/logs/rent-location.log"))

fmap fst .  PP.toListM' $  PA.parsed (parseMobsInLocation >>= \mobs -> clearColors *> pure mobs ) (loadServerEvents "test/logs/mobs-message.log" )
res <- fmap snd .  PP.toListM' $  PA.parsed (parseMobsInLocation) (loadServerEvents "test/logs/mobs-message.log" )
toListM $ fromJust (res ^? _Left . _2)

fmap L.nub $ PP.toListM $ PP.map (runReader (preview (_LocationEvent . _5 . traverse))) <-< PP.filter (has (_LocationEvent . _5 . _Just)) <-< serverLogEventsProducer

:{
  PP.toListM $
  PP.map (runReader (preview (_LocationEvent))) <-<
  PP.filter ((== (Just 4829)) . runReader (preview (_1 . traverse . _LocationEvent . _1 . locationId))) <-<
  scanZone <-<
  (parseServerEvents . loadServerEvents) "test/logs/little-bear-run.log"

:}

(encodePretty @([(Text , Maybe Text)]) . L.sortBy (\l r -> compare (snd l) (snd r)) . M.toList <$> loadCachedMobAliases) >>= LC8.writeFile "mob-aliases.list.json"


-- calculate unidentified mobs
mobsToIdentify <- (\im dm -> M.keys dm L.\\ M.keys im) <$> loadCachedMobData <*> loadCachedMobsOnMap

stack test --file-watch

g & run $ PP.print

g & run $ yield (SendToServer "постой") >> yield (SendToServer "0")

g & run $ forever await

g & runE $ eatTillFull >> drinkTillFull

g & run $ killEmAll world

g & runE $ prepareToRoam 

g & runE $ cover world

g & runE $ runZoneV3Field >> travelToLoc rentLocation world

g & runE $ travelToLoc "5100" world >> travel (zonePath world 5100) world

g & runE $ travelToLoc "4056" world 

runE g $ travelToLoc bankLocation world >> travelToLoc "5100" world >> (killEmAll world >-> travel (zonePath world 5100) world)

g & runE $ travelToLoc bankLocation world

g & runE $ travelToLoc rentLocation world

run g $ testParTasks world g

run g PP.drain

g & runE $ travelToLoc rentLocation world

. runTwo g (killEmAll world >> pure ()) ((runExceptP $ travel (zonePath world 5100) world) >> pure ())

(awaitLoc 3 (LocationId 5000))
(awaitLoc 2 (LocationId 5000))

:{
awaitLoc :: Int -> LocationId -> Pipe Event Event IO ()
awaitLoc taskId locIdToWait = inner
  where
    inner =
      await >>= \case
        (ServerEvent (LocationEvent (Event.Location (locId) _) _ _ _)) ->
          if locId == locIdToWait
            then Pipes.yield $ SendOnPulse taskId jump
            else inner
        _ -> inner
    jump = "говорить помогу" <> showt taskId
:}

--find connections of a location
(listFilesIn "/home/tort/mud-drifter/archive/server-input-log/" ) >>= (extractDirections . parseServerEvents . loadLogs) >>= traverse_ print . L.filter (\k -> (LocationId 5011 == fst k) || (LocationId 5011 == snd k)) . M.keys

putStrLn $ showPath world $ findTravelPath (LocationId 5000) (LocationId 5032) (_worldMap world)

printMobsByRegex world "рыбак"

zonePath world 5600

:{
  scanTree startLocId = PP.scan onEvent (False, Nothing) identity
    where
      onEvent _ e@(LocationEvent (Event.Location (LocationId locId) _) _ _ _) | locId == startLocId = (True, Just e)
      onEvent _ e@(LocationEvent (Event.Location (LocationId locId) _) _ _ _) | locId == 5000 = (False, Just e)
      onEvent _ EndOfLogEvent = (False, Just EndOfLogEvent)
      onEvent acc@(False, _) item = (False, Just item)
      onEvent acc@(True, Nothing) item = acc
      onEvent (True, _) item = (True, Just item)
:}

:{
checkFoodAmount :: MonadIO m => Pipe Event Event m Int
checkFoodAmount = yield (SendToServer "осм меш") >> getFoodCount
  where getFoodCount = await >>= \case (ServerEvent (ExamineContainer _ items)) -> return (countFood items)
                                       evt -> yield evt >> getFoodCount
        countFood :: [InventoryItem] -> Int
        countFood = sum . fmap itemToCount
        itemToCount (Single (ObjRef "ломоть хлеба") _) = 1
        itemToCount (Multiple (ObjRef "ломоть хлеба") n) = n
        itemToCount _ = 0
drinkTillFull :: MonadIO m => Pipe Event Event (ExceptT Text m) ()
drinkTillFull = drink 
  where drink = await >>= \e -> do
                            case e of PulseEvent -> yield (SendToServer drinkFromBarrel) >> awaitAction
                                      evt -> yield evt >> drink
        awaitAction = await >>= \e -> do
                            case e of ServerEvent DrinkFromAbsentObject -> lift $ throwError "liquid source is absent"
                                      ServerEvent (Drink _ _) -> awaitEffect
                                      ServerEvent (LiquidContainerIsEmpty) -> lift $ throwError "container is empty"
                                      ServerEvent NotThirsty -> return ()
                                      PulseEvent -> awaitAction
                                      evt -> yield evt >> awaitAction
        awaitEffect = await >>= \e -> do
                            case e of ServerEvent NotThirsty -> return ()
                                      ServerEvent PromptEvent{} -> drink
                                      PulseEvent -> awaitEffect
                                      evt -> yield evt >> awaitEffect
        drinkFromBarrel = "пить мех"
eatTillFull = eat
  where eat = await >>= \case PulseEvent -> do yield (SendToServer takeFoodFromContainer)
                                               yield (SendToServer eatFood)
                                               awaitAction
                              evt -> yield evt >> eat
        awaitAction = await >>= \case ServerEvent NotHungry -> ddo "полож все.хлеб мешок"
                                      ServerEvent (ItemAbsent _) -> lift . throwError $ "nothing to eat"
                                      ServerEvent (Eat _) -> awaitEffect
                                      PulseEvent -> awaitAction
                                      evt -> yield evt >> awaitAction
        awaitEffect = await >>= \case ServerEvent NotHungry -> return ()
                                      ServerEvent PromptEvent{} -> eat
                                      PulseEvent -> awaitEffect
                                      evt -> yield evt >> awaitEffect
        takeFoodFromContainer = "вз хлеб мешок"
        eatFood = "есть хлеб"
:}

:{
  questWhiteSpider world = do
    (killEmAll world >-> travelToLoc "5104" world)
    ddo "приставить лестница"
    killEmAll world >-> (travelToMob world (ObjRef "белый паук") >> pure ())
    (killEmAll world >-> travelToLoc "5119" world)
    ddo "отпер сунд"
    ddo "откр сундук"
    ddo "взять все сундук"
    ddo "смотреть"
    ddo "взять все"
:}

:{
prepareToRoam = do 
  travelToLoc tavernLoc world 
  checkFoodAmount >>= \n -> buyFood (20 - n)
  exceptP . fmap Right $ ddo "полож все.хлеб мешок"
  eatTillFull
  travelToLoc wellLoc world
  exceptP . fmap Right $ ddo "пить колодец"
  exceptP . fmap Right $ ddo "наполнить мех колодец"
buyFood n | n > 0 = (exceptP . fmap Right $ ddo ("купить " <> showt n <> " 1"))
          | otherwise = return ()
questV1SwampFlower = do
    travelToLoc "4318" world
    travelToLoc "4343" world
    (exceptP . fmap Right $ ddo "взять цветок")
    travelToLoc "4318" world
    (exceptP . fmap Right $ ddo "дать цветок старик")
questOldBoots = do
      (exceptP . fmap Right $ killEmAll world) >-> travelToLoc "5112" world
       fmap Right $ ddo "снять нож" >> ddo holdCandle)
      Pipes.Lift.catchError ( do
        (exceptP . fmap Right $ cover world) >-> travelToLoc chestLocation world
        (exceptP . fmap Right $ ddo takeAllFromChest)
        (exceptP . fmap Right $ cover world) >-> travelToLoc "5112" world >> return ()) $ liftIO . printT
      (exceptP . fmap Right $ ddo unholdCandle >> ddo "держ нож")
questFisherman = do
  (exceptP . fmap Right $ killEmAll world) >-> (travelToMob world . MobRoomDesc $ "Старый рыбак чинит здесь свою сеть.")
  (exceptP . fmap Right $ ddo "говорить помогу")
  travelAndKill world allKillableMobs $ MobRoomDesc "Злобная щука плавает здесь."
  (exceptP . fmap Right $ killEmAll world) >-> (travelToMob world . MobRoomDesc $ "Старый рыбак чинит здесь свою сеть.")
  (exceptP . fmap Right $ ddo "дать филе рыбак")
runZoneV2AntHill = runZone world allKillableMobs "5302" 5300
runZoneV2Lake = runZone world allKillableMobs "55601" 5600
runZoneRaspberryGarden = runZone world allKillableMobs "4801" 4800
runZoneV1Lake = runZone world allKillableMobs "4301" 4300
runZoneV2Forest = runZone world allKillableMobs "5401" 5400
runZoneV3Garden = runZone world allKillableMobs "6601" 6600
runZoneAbandonedHouse = runZone world allKillableMobs "5101" 5100
runZoneV3Field = runZone world allKillableMobs "6701" 6700
:}

:{
initAliasCache =
  (encodePretty @(Map Text (Maybe Text)) <$> (M.union <$> knownMobs <*> allMobs)) >>=
  LC8.writeFile "cache/mob-aliases.json"
  where
    allMobs :: IO (Map Text (Maybe Text))
    allMobs = M.fromList . fmap ((, Nothing) . unObjRef) . M.keys <$> loadCachedMobsOnMap
    knownMobs :: IO (Map Text (Maybe Text))
    knownMobs = M.fromList . fmap (bimap unObjRef (fmap unObjRef . _alias . _nameCases)) . M.assocs <$> loadCachedMobData
:}
