:{
  task :: Producer () IO ()
  task = liftIO $ threadDelay $ 1*1000*1000
  --in replicateM_ 5 task
:}

:set -XLambdaCase

:set -XDataKinds

import TextShow

import Pipes.Lift

import Data.Map.Strict ((!))

import qualified Data.Map.Strict as M

import qualified Data.List as L

import qualified Data.Set as S

import qualified Data.Text as T

import qualified Pipes.Prelude as PP

import Control.Lens

:{
let rentLocation = "5000"
    bankLocation = "5032"
    chestLocation = "5114"
    tavernLoc = "6000"
    wellLoc = "6035"
    abandonedHouseEntrance = 5100
    holdCandle = "держать свечка"
    unholdCandle = "снять свечка"
    takeAllFromChest = "взять все ящик"
    ddo :: Monad m => Text -> Pipe Event Event m ()
    ddo = yield . SendToServer
    checkCases mob = mapM_ (ddo) . fmap (<> " " <> mob) $ ["благословить", "думать", "бояться", "бухать", "хваст", "указать"]
    mobStats :: Text -> Text -> (ObjRef Mob InRoomDesc, MobStats)
    mobStats desc alias = (ObjRef desc, nameCases .~ cases $ mempty)
      where cases = inRoomDesc ?~ (ObjRef desc) $ mempty
    hardcodedProperties :: Map (ObjRef Mob InRoomDesc) MobStats
    hardcodedProperties = M.fromList [ mobStats "Серая мышь юркнула здесь." "мышь.серая"
                                     , mobStats "(летит) Веселая пчелка собирает пыльцу." "пчела"
                                     , mobStats "Мышь-полевка быстро бежит к своей норке." "мышка"
                                     , mobStats "(летит) Большой рой комаров мешает тут Мише." "рой.комар"
                                     , mobStats "Речная крыса прошмыгнула у Ваших ног." "ондатра"
                                     , mobStats "Небольшая безногая ящерица проскользнула у Ваших ног." "медянка"
                                     , mobStats "(летит) Маленький воробей летает здесь." "воробей"
                                     , mobStats "Огородник-работяга ухаживает за огородом." "огородник"
                                     , mobStats "Мышь-полевка бегает здесь." "полевка"
                                     , mobStats "Большой дикий бык величаво пережевывает траву здесь." "бык"
                                     ]
:}

:{
  isWhiteSpiderEvent LocationEvent{} = True
  isWhiteSpiderEvent (HitEvent (ObjRef "Белый паук") _)  = True
  isWhiteSpiderEvent (HitEvent _ (ObjRef "белого паука"))  = True
  isWhiteSpiderEvent (FightPromptEvent _ _) = True
  isWhiteSpiderEvent _ = False
  renderHitEvents (HitEvent from to) = showt from <> " : " <> showt to
  renderHitEvents (LocationEvent loc _ _ _) = showt loc
  renderHitEvents FightPromptEvent{} = "FIGHT PROMPT"
:}

:{
  render (l, r) = showt l <> " : " <> showt r
  toStats [FightPromptEvent _ target, IHitMobEvent _, PromptEvent _ _, loc@(LocationEvent _ _ [mob] _)] = (mob, target)
  toStats2 [FightPromptEvent _ target, HitEvent _ (ObjRef "вас"), loc@(LocationEvent _ _ [mob] _)] = (mob, target)
  attackLonelyMob [FightPromptEvent _ target, IHitMobEvent _, PromptEvent _ _, loc@(LocationEvent _ _ [mob] _)] = True
  attackLonelyMob _ = False
  lonelyMobAttacksMe [FightPromptEvent _ target, HitEvent _ (ObjRef "вас"), loc@(LocationEvent _ _ [mob] _)] = True
  lonelyMobAttacksMe _ = False
  scanWindow n = PP.scan toWindow [] identity
    where toWindow acc event
            | P.length acc < n = event : acc
            | otherwise = event : P.take (n - 1) acc
:}

stack test --file-watch

mapM_ putStrLn . S.toList . S.fromList =<< (PP.toListM $ PP.map (render . toStats) <-< PP.filter attackLonelyMob <-< scanWindow 4 <-< serverLogEventsProducer )

mapM_ putStrLn . S.toList . S.fromList =<< (PP.toListM $ PP.map (render . toStats2) <-< PP.filter lonelyMobAttacksMe <-< scanWindow 3 <-< serverLogEventsProducer )

runEffect $ PP.mapM_ putStrLn <-< PP.map renderHitEvents <-< PP.filter isWhiteSpiderEvent <-< serverLogEventsProducer

world <- loadWorld "/home/tort/mud-drifter/" hardcodedProperties

import Person

import World

genod = Person { personName = "генод"
               , personPassword = "каркасный"
               , residence = MudServer "bylins.su" 4000
               }

g <- initPerson genod

g & run $ login

g & run $ PP.print

g & run $ yield (SendToServer "постой") >> yield (SendToServer "0")

g & run $ forever await

g & runE $ eatTillFull >> drinkTillFull

g & run $ killEmAll world

g & runE $ prepareToRoam 

g & runE $ cover world

g & runE $ runZoneV3Field >> travelToLoc rentLocation world

g & runE $ travelToLoc rentLocation world

run g $ testParTasks world g

g & runE $ travelToLoc bankLocation world

:{
runTwo :: (Output ByteString, Input Event) -> IO ()
runTwo person@(personOut, personIn) =
  spawn' (newest 100) >>= \(subtaskOut1, subtaskIn1, seal1) ->
    spawn' (newest 100) >>= \(subtaskOut2, subtaskIn2, seal2) ->
      (async $
       waitLocation (LocationId 5000) (personOut, subtaskIn1) >>
       print "subtask1 finished" >>
       (atomically seal1) >>
       (atomically seal2)) >>
      (async $
       waitLocation (LocationId 5032) (personOut, subtaskIn2) >>
       print "subtask2 finished" >>
       (atomically seal1) >>
       (atomically seal2)) >>
      (runEffect $
       fromInput personIn >-> toOutput (subtaskOut1 <> subtaskOut2) >>
       print "read finished") >>
      pure ()
waitLocation :: LocationId -> (Output ByteString, Input Event) -> IO ()
waitLocation locIdToWait (outChan, inChan) = runEffect $ fromInput inChan >-> awaitLoc
  where
    awaitLoc =
      await >>= \case
        (ServerEvent (LocationEvent (Event.Location (locId) _) _ _ _)) ->
          if locId == locIdToWait
            then liftIO $ sendOutput jump
            else awaitLoc
        _ -> awaitLoc
    sendOutput text = do
      atomically $ PC.send outChan (encodeUtf8 . T.snoc text $ '\n')
      pure ()
    jump = "говорить помогу"
:}

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
questWhiteSpider = do
  Pipes.Lift.catchError ( do 
    (exceptP . fmap Right $ killEmAll world) >-> travelToLoc "5104" world
    (exceptP . fmap Right $ ddo "приставить лестница")
    (exceptP . fmap Right $ cover world) >-> (travelAndKill world allKillableMobs $ MobRoomDesc "Большой паук с белым брюхом ждет жертву здесь.")
    travelToLoc "5119" world
    (exceptP . fmap Right $ ddo "отпер сунд" >> ddo "откр сундук" >> ddo "взять все сундук" >> ddo "смотреть" >> ddo "взять все" >> return "all good")) $ return
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
