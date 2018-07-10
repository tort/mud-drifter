{-# LANGUAGE OverloadedStrings #-}

module Person ( runPerson
              , loadWorld
              , parseProducer
              ) where

import Control.Applicative ((<|>))
import Data.Monoid
import Data.Char
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as DTIO
import Reactive.Banana hiding (Event)
import qualified Reactive.Banana as B
import Reactive.Banana.Frameworks
import Data.Maybe
import qualified Network.Simple.TCP as NST
import qualified Network.Socket.ByteString as NBS
import qualified Network.Socket as NS
import Pipes
import Pipes.Concurrent
import qualified Pipes.Concurrent as PC
import Pipes.Network.TCP as PNT
import System.IO (hClose, openFile, Handle, withFile, IOMode(..))
import Data.Text.Encoding
import Data.ByteString.Char8 as DBC8 hiding (isPrefixOf, snoc, putStrLn)
import Control.Concurrent.Async
import ServerInputParser
import Data.Attoparsec.ByteString as A
import Pipes.Attoparsec
import qualified Pipes.Attoparsec as PA
import Pipes.Parse
import qualified Data.Configurator as DC
import Data.Configurator.Types
import Mapper
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph as G
import qualified Pipes.ByteString as PBS
import qualified Data.Foldable as F
import System.Directory
import Data.Either.Utils
import Data.Text.Read
import Text.Parsec
import qualified Text.Parsec as Parsec
import UserInputParser
import qualified Data.Graph.Inductive.Query.SP as GA
import Data.Set hiding ((\\))
import qualified Data.Set as S
import Data.List
import qualified Data.List as L
import Prelude
import qualified Prelude as P
import Debug.Trace
import Event
import qualified Event as E
import Control.Monad
import qualified Pipes.Safe as PS
import Control.Concurrent.Timer
import Control.Concurrent.Suspend.Lifted
import Data.Map hiding ((\\), mapAccum)
import qualified Data.Map as M
import Logger

data MoveRequest = MoveRequest TaskKey Location
type TaskKey = Int

runPerson :: World -> EventBus -> IO ()
runPerson world eventBus = do
    network <- compile $ personTask world eventBus
    actuate network

personTask :: World -> EventBus -> MomentIO ()
personTask world outerBus = do
  let graph = buildMap $ directions world
  (innerBus, triggerInnerEvent) <- newEvent
  liftIOLater (runPulses triggerInnerEvent)
  mapOuterEventsToInner (snd outerBus) triggerInnerEvent
  firePersonCommands outerBus innerBus triggerInnerEvent
  bSocket <- keepConnectedTask innerBus triggerInnerEvent (fst outerBus)
  keepLoggedTask innerBus triggerInnerEvent (fst outerBus)
  redirectNonPersonCommandsToServerTask innerBus triggerInnerEvent
  fireServerEventsTask outerBus innerBus triggerInnerEvent
  bCurrentLoc <- mapperTask world graph (fst outerBus) innerBus
  evts <- travelTask world graph bCurrentLoc innerBus triggerInnerEvent
  sendServerCommandTask bSocket evts (fst outerBus)
  updateEquipTask innerBus triggerInnerEvent (fst outerBus)
  return ()

runPulses :: Handler Event -> IO ()
runPulses triggerEvent = do repeatedTimer emitPulse (msDelay 1000)
                            return ()
  where emitPulse = triggerEvent PulseEvent

updateEquipTask :: B.Event E.Event -> Handler E.Event -> Output E.Event -> MomentIO ()
updateEquipTask event triggerEvent toOuterBus = do
  let identifiedItems = []
  equippedItems <- checkEquipTask
  inventoryItems <- checkInventory
  containerItems <- checkContainer
  shopItems <- checkShop
  equipItems event triggerEvent =<< getItems =<< itemsToGet identifiedItems equippedItems inventoryItems containerItems shopItems
  return ()
    where checkEquipTask = return ()
          checkInventory = return ()
          checkContainer = return ()
          checkShop = return ()

itemsToGet :: [a] -> () -> () -> () -> () -> MomentIO (B.Event EquippedItem)
itemsToGet identifiedItems equippedItems inventoryItems containerItems shopItems = do
  (itemEvt, triggerItemEvt) <- newEvent
  return itemEvt

getItems :: B.Event EquippedItem -> MomentIO ((B.Event E.Event, B.Event EquippedItem))
getItems itemToGetEvent = do
  (gotItemEvent, triggerEvt) <- newEvent
  (pulseEvent, triggerPulse) <- newEvent
  return (gotItemEvent, pulseEvent)

equipItems :: B.Event E.Event -> Handler E.Event -> (B.Event E.Event, B.Event EquippedItem) -> MomentIO (B.Event E.Event)
equipItems events triggerEvent (pulseEvent, itemsEvent) = do
  eithers <- accumE (Left []) $ B.unions [ accumItem <$> itemsEvent
                                         , accumPulse <$> pulseEvent
                                         ]
  let (unusedTickEvent, equipItemEvent) = B.split eithers
  reactimate $ equipListAction <$> equipItemEvent
  return $ PulseEvent <$ unusedTickEvent
    where accumPulse PulseEvent = \acc -> case acc of (Left []) -> Left []
                                                      (Left acc) -> Right acc
                                                      (Right _) -> Left []
          accumItem item@(EquippedItem _ _) = \acc -> case acc of (Left _) -> (++[item]) <$> acc
                                                                  (Right _) -> Left [item]
          equipServerCommand (EquippedItem slot itemName) = ServerCommand $ equipCommand slot <> " " <> (replace " " "." itemName)
          equipItemAction item = triggerEvent $ equipServerCommand item :: IO ()
          equipListAction items = do mapM_ equipItemAction items :: IO ()
                                     case items of [] -> return ()
                                                   xs -> triggerEvent listEquipCmd


equipCommand :: Slot -> Text
equipCommand Wield = "вооруж"
equipCommand Hold = "держ"
equipCommand DualWield = "вооруж две"
equipCommand _ = "одеть"

checkMissingItems :: B.Event Event -> Handler Event -> B.Event Event -> MomentIO (B.Event [EquippedItem])
checkMissingItems event triggerEvent checkEquipEvent = return missingEquipmentEvent
    where missingEquipmentEvent = missingEquipment <$> filterE isListEquipEvent event
          missingEquipment (ServerEvent (ListEquipmentEvent l)) = equipPlan \\ (fst <$> l)
          itemName (EquippedItem _ name) = name
          equipPlan = [ EquippedItem Wield "длинный бронзовый меч"
                      , EquippedItem Hold "бронзовый топорик"
                      , EquippedItem Body "легкий латный доспех"
                      , EquippedItem Head "легкий латный шлем"
                      , EquippedItem Legs "легкие латные поножи"
                      ]
          isListEquipEvent (ServerEvent (ListEquipmentEvent _)) = True
          isListEquipEvent _ = False

listEquipCmd :: E.Event
listEquipCmd = ServerCommand "экипировка"

mapOuterEventsToInner :: Input E.Event -> Handler E.Event -> MomentIO ()
mapOuterEventsToInner fromOuterBus triggerInnerEvent = do
  liftIOLater $ wrapAsync $ do runEffect $ fromInput fromOuterBus >-> fireEventConsumer triggerInnerEvent
                               performGC

fireServerEventsTask :: EventBus -> B.Event E.Event -> Handler E.Event -> MomentIO ()
fireServerEventsTask outerBus innerEvent triggerEvent = do
    (serverEvent, x) <- mapAccum Nothing $ scanServerInput <$> filterE isServerInput innerEvent
    reactimate $ triggerEventAction <$> serverEvent
    where isServerInput (ServerInput _) = True
          isServerInput _ = False
          triggerEventAction evt = mapM_ fireOuterEvent evt
          fireOuterEvent evt = do atomically $ PC.send (fst outerBus) evt
                                  return ()

scanServerInput :: E.Event -> Maybe (Result ServerEvent) -> ([E.Event], Maybe (Result ServerEvent))
scanServerInput (ServerInput text) Nothing = parseWholeServerInput (A.parse serverInputParser text) []
scanServerInput (ServerInput "") _ = ([], Nothing)
scanServerInput (ServerInput text) (Just (Partial cont)) = parseWholeServerInput (cont text) []

parseWholeServerInput :: Result ServerEvent -> [E.Event] -> ([E.Event], Maybe (Result ServerEvent))
parseWholeServerInput (Done "" r) events = (events ++ [ServerEvent r], Nothing)
parseWholeServerInput (Done remaining evt) events = let nextResult = A.parse serverInputParser remaining
                                                     in parseWholeServerInput nextResult (events ++ [ServerEvent evt])
parseWholeServerInput cnt@(Partial cont) events = (events, Just cnt)
parseWholeServerInput (Fail remaining contexts desc) events = (events ++ [errorEvt contexts desc], Nothing)
  where errorEvt contexts desc = ConsoleOutput ("parsing error: " <> DBC8.pack desc <> " contexts: " <> (DBC8.unwords $ DBC8.pack <$> contexts))

fireEventConsumer :: Handler E.Event -> Consumer E.Event IO ()
fireEventConsumer fireEvent = forever $ do
    event <- await
    liftIO $ wrapAsync $ fireEvent event


fireServerEventConsumer :: Handler E.Event -> Consumer (Maybe (Either ParsingError ServerEvent)) IO ()
fireServerEventConsumer fireServerEvent = do
    event <- await
    liftIO $ handleEvent event
    fireServerEventConsumer fireServerEvent
    where handleEvent Nothing = return ()
          handleEvent (Just (Left err)) = return ()
          handleEvent (Just (Right event)) = fireServerEvent $ ServerEvent event

redirectNonPersonCommandsToServerTask :: B.Event E.Event -> Handler E.Event -> MomentIO ()
redirectNonPersonCommandsToServerTask event trigger = reactimate $ (wrapAsync . triggerRedirect) <$> filterE isRedirectCommand event
  where triggerRedirect (PersonCommand (UserInputRedirect txt)) = do trigger $ ServerCommand txt
        triggerRedirect _ = return ()
        isRedirectCommand (PersonCommand (UserInputRedirect _)) = True
        isRedirectCommand _ = False

firePersonCommands :: EventBus -> B.Event E.Event -> Handler E.Event -> MomentIO ()
firePersonCommands (toOuterBus, _) innerEvent triggerInnerEvent = do
  reactimate $ handleConsoleInput <$> innerEvent
    where handleConsoleInput (ConsoleInput text) = case Parsec.parse userInputParser "" text of (Right cmd) -> triggerInnerEvent $ PersonCommand cmd
                                                                                                (Left err) -> do atomically $ PC.send toOuterBus $ ConsoleOutput $ DBC8.pack $ show err
                                                                                                                 return ()
          handleConsoleInput _ = return ()

sendServerCommandTask :: Behavior (Maybe Socket) -> B.Event E.Event -> Output Event -> MomentIO ()
sendServerCommandTask bSocket innerEvent toOuterBus = do
  let evtTxt = (\(ServerCommand txt) -> txt) <$> filterE isServerCommand innerEvent
  reactimate $ sendCommand toOuterBus <$> bSocket <@> evtTxt

isServerCommand :: E.Event -> Bool
isServerCommand (ServerCommand _) = True
isServerCommand _ = False

sendCommand :: Output Event -> Maybe Socket -> Text -> IO ()
sendCommand _ (Just sock) txt = do async $ NST.send sock $ encodeUtf8 $ snoc txt '\n'
                                   return ()
sendCommand toOuterBus Nothing _ = do async $ atomically $ PC.send toOuterBus $ ConsoleOutput "no connection to server\n"
                                      return ()

keepLoggedTask :: B.Event E.Event -> Handler E.Event -> Output E.Event -> MomentIO ()
keepLoggedTask serverEvent triggerEvent toOuterBus = do
    reactimate $ (wrapAsync $ triggerEvent codepageServerCommand) <$ filterE (== (ServerEvent CodepagePrompt)) serverEvent
    reactimate $ (wrapAsync $ triggerEvent emptyServerCommand) <$ filterE (== (ServerEvent WelcomePrompt)) serverEvent
    reactimate $ (wrapAsync $ loadLoginAndSendCommand) <$ filterE (== (ServerEvent LoginPrompt)) serverEvent
    reactimate $ (wrapAsync $ loadPasswordAndSendCommand) <$ filterE (== (ServerEvent PasswordPrompt)) serverEvent
      where loadLoginAndSendCommand = handle =<< loadConfigProperty "person.login"
                                                             where handle (Just login) = triggerEvent $ ServerCommand login
                                                                   handle Nothing = sendToConsole toOuterBus "failed to load person login\n"
            loadPasswordAndSendCommand = handle =<< loadConfigProperty "person.password"
                                                                where handle (Just pass) = triggerEvent $ ServerCommand pass
                                                                      handle Nothing = sendToConsole toOuterBus "failed to load person password\n"

keepConnectedTask :: B.Event E.Event -> Handler E.Event -> Output E.Event -> MomentIO (Behavior (Maybe Socket))
keepConnectedTask event triggerEvent toOuterBus = do
    let keepConnectionEvent = keepConnectionCommandToEvent <$> filterE isKeepConnectionCommand event
        disconnectionEvent = filterE (== ServerDisconnection) event
    bKeepConnection <- stepper False keepConnectionEvent
    (bSocket, fireUpdateSocket) <- newBehavior Nothing

    reactimate $ (wrapAsync $ connectToServer fireUpdateSocket toOuterBus) <$ whenE (isNothing <$> bSocket) (filterE id keepConnectionEvent)
    reactimate $ (wrapAsync $ connectToServer fireUpdateSocket toOuterBus) <$ whenE bKeepConnection disconnectionEvent
    return bSocket

wrapAsync :: IO a -> IO ()
wrapAsync l = do async l
                 return ()

connectToServer :: Handler (Maybe Socket) -> Output E.Event -> IO ()
connectToServer updateSocketBehavior toOuterBus = do
  async $ PS.runSafeT $ do runEffect $ readSocket
  return ()
    where onRun (sock, _, (logChanOutput, _, _)) = fromSocket sock (2^15) >-> serverInputConsumer toOuterBus logChanOutput >> onPipeClose >> liftIO (sendToConsole toOuterBus "socket channel finished\n")
          onCreate = do (sock, _) <- NST.connectSock "bylins.su" "4000"
                        updateSocketBehavior $ Just sock
                        hLog <- openFile "server-input.log" WriteMode
                        logChan@(_, logChanInput, _) <- spawn' $ newest 1024
                        async $ runEffect $ fromInput logChanInput >-> PBS.toHandle hLog
                        return (sock, hLog, logChan)
          onDestroy (sock, hLog, (_, _, seal)) =  do NST.closeSock sock
                                                     hClose hLog
                                                     atomically seal
          onPipeClose = (liftIO $ updateSocketBehavior Nothing) >> indicateEndOfInput >> fireDisconnectionEvent
          fireDisconnectionEvent = liftIO $ wrapAsync $ atomically $ PC.send toOuterBus ServerDisconnection
          readSocket = PS.bracket onCreate onDestroy onRun
          indicateEndOfInput = liftIO $ do async $ do atomically $ PC.send toOuterBus $ ServerInput ""
                                           return ()

serverInputConsumer :: MonadIO m => Output E.Event -> Output ByteString -> Consumer ByteString m ()
serverInputConsumer toOuterBus toLog = do text <- await
                                          liftIO $ do async $ do atomically $ PC.send toOuterBus $ ServerInput text
                                                      return ()
                                          liftIO $ do async $ do atomically $ PC.send toLog $ text
                                                      return ()
                                          serverInputConsumer toOuterBus toLog

keepConnectionCommandToEvent :: E.Event -> Bool
keepConnectionCommandToEvent (PersonCommand (KeepConn v)) = v

isKeepConnectionCommand :: E.Event -> Bool
isKeepConnectionCommand (PersonCommand (KeepConn _)) = True
isKeepConnectionCommand _ = False

sendToConsole :: Output E.Event -> ByteString -> IO ()
sendToConsole toOuterBus text = do atomically $ PC.send toOuterBus $ ConsoleOutput text
                                   return ()

codepageServerCommand :: E.Event
codepageServerCommand = ServerCommand "5"

emptyServerCommand :: E.Event
emptyServerCommand = ServerCommand ""

loadConfigProperty :: Text -> IO (Maybe Text)
loadConfigProperty propertyName = do conf <- DC.load [Required personCfgFileName]
                                     propertyValue <- DC.lookup conf propertyName
                                     return propertyValue

personCfgFileName :: String
personCfgFileName = "person.cfg"

mapperTask :: World -> Gr () Int -> Output E.Event -> B.Event E.Event -> MomentIO (Behavior (Maybe LocId))
mapperTask world graph toOuterBus innerEvent = do
  bLoc <- stepper Nothing $ changeCurrLocEvent innerEvent
  let findPathResponse = showFindPathResponse world graph <$> bLoc <@> filterE isFindPath innerEvent
  reactimate $ (sendToConsole toOuterBus . matchingLocs world) <$> filterE isFindLoc innerEvent
  reactimate $ sendToConsole toOuterBus <$> findPathResponse
  return bLoc

buildMap :: Set Direction -> Gr () Int
buildMap directions = mkGraph nodes edges
  where edges = F.concat $ (\d -> aheadEdge d : reverseEdge d : []) <$> (S.toList directions)
        nodes = fmap (\n -> (n, ())) $ F.foldl (\acc d -> locIdFrom d : locIdTo d : acc) [] directions
        aheadEdge d = (locIdFrom d, locIdTo d, 1)
        reverseEdge d = (locIdTo d, locIdFrom d, 1)

isFindPath :: E.Event -> Bool
isFindPath (PersonCommand (FindPathFromTo from to)) = True
isFindPath (PersonCommand (FindPathToLocId to)) = True
isFindPath (PersonCommand (FindPathTo to)) = True
isFindPath _ = False

showFindPathResponse :: World -> Gr () Int -> Maybe Int -> E.Event -> ByteString
showFindPathResponse world graph currLoc userInput =
  let destination = locsByRegex world
      showPathBy f t = if (f == t) then "you are already there!"
                           else (showPath $ directions world) $ GA.sp f t graph
   in case (currLoc, userInput) of
        (_, (PersonCommand (FindPathFromTo from to))) -> showPathBy from to
        (Just currLoc, (PersonCommand (FindPathToLocId to))) -> showPathBy currLoc to
        (Just currLoc, (PersonCommand (FindPathTo regex))) -> let matchingLocs = destination regex
                                             in case S.toList $ matchingLocs of
                                                  [] -> "no matching locations found"
                                                  d:[] -> showPathBy currLoc (locId d)
                                                  _ -> showLocs matchingLocs
        (Nothing, _) -> "current location is unknown\n"


showPath :: Set Direction -> Path -> ByteString
showPath directions [] = "path is empty\n"
showPath directions path = (encodeUtf8 . addRet . joinToOneMsg) (showDirection . (nodePairToDirection directions) <$> toJust <$> nodePairs)
  where joinToOneMsg = T.intercalate "\n"
        showDirection = trigger
        addRet txt = T.snoc txt '\n'
        nodePairs = filterDirs $ P.scanl (\acc item -> (snd acc, Just item)) (Nothing, Nothing) path
        filterDirs = P.filter (\pair -> isJust (fst pair) && isJust (snd pair))
        toJust (Just left, Just right) = (left, right)

nodePairToDirection :: Set Direction -> (LocId, LocId) -> Direction
nodePairToDirection directions (from, to) = P.head $ S.toList $ S.filter (\d -> locIdFrom d == from && locIdTo d == to) directions

isFindLoc :: E.Event -> Bool
isFindLoc (PersonCommand (FindLoc regex)) = True
isFindLoc _ = False

matchingLocs :: World -> E.Event -> ByteString
matchingLocs graph (PersonCommand (FindLoc regex)) = showLocs $ locsByRegex graph regex

questEvent :: B.Event E.Event -> MomentIO (B.Event E.Event)
questEvent innerEvent = do
  (bQuestChain, updateQuestChain) <- newBehavior []
  (outEvt, triggerOutEvt) <- newEvent
  let questAction [] = triggerOutEvt PulseEvent
  reactimate $ questAction <$> bQuestChain <@ filterE (== PulseEvent) innerEvent
  return $ B.unionWith (\l r -> l) outEvt (filterE (/= PulseEvent) innerEvent)

travelTo :: World -> B.Event E.Event -> MomentIO (B.Event E.Event)
travelTo world innerEvent = do
  (bPath, updatePath) <- newBehavior []
  (outEvt, triggerOutEvt) <- newEvent
  qe <- questEvent innerEvent
  let onChangeLocAction [] _ = return ()
      onChangeLocAction path (ServerEvent (LocationEvent loc _))  = updatePath $ if (L.elem (locId loc) path) then L.dropWhile (/= (locId loc)) path
                                                                                                              else path
      moveAction path = moveActionFromPath path
        where moveActionFromPath (x:y:xs) = triggerOutEvt $ moveCommand x y
              moveActionFromPath _ = triggerOutEvt $ PulseEvent
              moveCommand fromLoc toLoc = ServerCommand $ trigger $ nodePairToDirection (directions world) (fromLoc, toLoc)
  reactimate $ onChangeLocAction <$> bPath <@> filterE isLocation qe
  reactimate $ (\(TravelRequest path) -> updatePath path) <$> filterE isTravelRequestPath qe
  reactimate $ moveAction <$> bPath <@ filterE (== PulseEvent) qe
  return $ B.unionWith (\l r -> l) outEvt (filterE (/= PulseEvent) qe)
    where isTravelRequestPath (TravelRequest _) = True
          isTravelRequestPath _ = False
          questActs fromLoc toLoc = M.lookup (fromLoc, toLoc) (questActions world)

travelTask :: World -> Gr () Int -> Behavior (Maybe LocId) -> B.Event E.Event -> Handler E.Event -> MomentIO (B.Event E.Event)
travelTask world graph currentLoc innerEvent triggerEvent = do
  outEvts <- travelTo world $ B.unionWith (\l r -> l) travelRequestEvt nonTravelRequestEvt
  return outEvts
    where isGoRequest (PersonCommand (GoToLocId _)) = True
          isGoRequest _ = False
          travelRequest Nothing (PersonCommand (GoToLocId locTo)) = TravelRequest []
          travelRequest (Just currentLocId) (PersonCommand (GoToLocId locTo)) = TravelRequest $ GA.sp currentLocId locTo graph
          travelRequestEvt = travelRequest <$> currentLoc <@> filterE isGoRequest innerEvent
          nonTravelRequestEvt = filterE (not . isGoRequest) innerEvent

changeCurrLocEvent :: B.Event E.Event -> B.Event (Maybe LocId)
changeCurrLocEvent innerEvent = changeCurrLocEvent
  where locEvent = filterE isLocEvent innerEvent
        isLocEvent (ServerEvent (LocationEvent _ _)) = True
        isLocEvent (ServerEvent DarknessEvent) = True
        isLocEvent _ = False
        changeCurrLocEvent = toLocId <$> locEvent
        toLocId (ServerEvent (LocationEvent loc _)) = Just $ locId loc
        toLocId (ServerEvent DarknessEvent) = Nothing

addRequestEvent :: B.Event MoveRequest -> B.Event ([(TaskKey, Location)] -> [(TaskKey, Location)])
addRequestEvent moveRequest = (\mr@(MoveRequest k dest) acc -> (k, dest) : acc) <$> moveRequest

--clarify if darkness should be included
isLocation :: E.Event -> Bool
isLocation (ServerEvent (LocationEvent _ _)) = True
isLocation _ = False

isMove :: E.Event -> Bool
isMove (ServerEvent (MoveEvent _)) = True
isMove _ = False

loadDirections :: IO (Set Direction) -> FilePath -> IO (Set Direction)
loadDirections ioDirs file = do
  hLog <- openFile file ReadMode
  dirs <- ioDirs
  newDirs <- foldToDirections dirs $ parseProducer (PBS.fromHandle hLog)
  hClose hLog
  return newDirs

loadLocations :: IO (Set Location) -> FilePath -> IO (Set Location)
loadLocations ioLocs file = do
  hLog <- openFile file ReadMode
  locs <- ioLocs
  newLocations <- foldToLocations locs $ parseProducer (PBS.fromHandle hLog)
  hClose hLog
  return newLocations

loadItems :: IO (Set Item) -> FilePath -> IO (Set Item)
loadItems accIO file = do
  hLog <- openFile file ReadMode
  items <- accIO
  result <- foldToItems items $ parseProducer (PBS.fromHandle hLog)
  hClose hLog
  return result

loadQuestActions :: IO (Map (Int, Int) [Event]) -> FilePath -> IO (Map (Int, Int) [Event])
loadQuestActions accIO file = withLog file $ obstacleActions . filterQuestEvents

loadWorld :: FilePath -> IO World
loadWorld archiveDir = do
  serverLogFiles <- listDirectory serverLogDir
  evtLogFiles <- listDirectory evtLogDir
  directions <- loadDirs serverLogFiles
  locations <- loadLocs serverLogFiles
  items <- loadItms serverLogFiles
  questActions <- loadQuestActs evtLogFiles
  return $ World locations directions items questActions
    where loadDirs files = F.foldl (\acc item -> loadDirections acc (serverLogDir ++ item)) (return S.empty) files
          loadLocs files = F.foldl (\acc item -> loadLocations acc (serverLogDir ++ item)) (return S.empty) files
          loadItms files = F.foldl (\acc item -> loadItems acc (serverLogDir ++ item)) (return S.empty) files
          loadQuestActs files = F.foldl (\acc item -> loadQuestActions acc (evtLogDir ++ item)) (return M.empty) files
          serverLogDir = archiveDir ++ "server-input-log/"
          evtLogDir = archiveDir ++ "evt-log/"

parseProducer :: Producer ByteString IO () -> Producer (Maybe (Either ParsingError ServerEvent)) IO ()
parseProducer src = do
    (result, partial) <- liftIO $ runStateT (PA.parse serverInputParser) src
    continue result partial
    where continue result@(Just (Right _)) partial = do yield result
                                                        parseProducer partial
          continue (Just (Left (ParsingError ctxts err))) _ = liftIO $ DBC8.putStr $ "error: " <> (DBC8.pack err) <> (DBC8.pack $ P.concat ctxts) <> "\n"
          continue Nothing _ = liftIO $ DBC8.putStr "parsed entire stream\n"
