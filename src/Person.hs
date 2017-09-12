{-# LANGUAGE OverloadedStrings #-}

module Person ( runPerson
              , loadWorld
              , SendToConsolesAction
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
import Data.ByteString.Char8 as DBC8 hiding (isInfixOf, isPrefixOf, snoc, putStrLn)
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
import Data.Set
import qualified Data.Set as S
import Prelude
import qualified Prelude as P
import Debug.Trace
import Event
import qualified Event as E
import Control.Monad (forever)

newtype GoDirectionAction = GoDirectionAction Text
data MoveRequest = MoveRequest TaskKey Location
type TaskKey = Int
type SendToConsolesAction = ByteString -> IO ()

newtype KeepConnectionCommand = KeepConnectionCommand Bool

runPerson :: World -> EventBus -> IO ()
runPerson world eventBus = do
    network <- compile $ personTask world eventBus
    actuate network

personTask :: World -> EventBus -> MomentIO ()
personTask world outerBus = do
  (innerBus, triggerInnerEvent) <- newEvent
  mapOuterEventsToInner (snd outerBus) triggerInnerEvent
  firePersonCommands outerBus innerBus triggerInnerEvent
  bSocket <- keepConnectedTask innerBus triggerInnerEvent (fst outerBus)
  keepLoggedTask innerBus triggerInnerEvent (fst outerBus)
  sendServerCommandTask bSocket innerBus (fst outerBus)
  redirectNonPersonCommandsToServerTask innerBus triggerInnerEvent
  fireServerEventsTask innerBus triggerInnerEvent
  {--mapperTask world consoleCommandEvent sendToConsolesAction--}
  return ()

mapOuterEventsToInner :: Input E.Event -> Handler E.Event -> MomentIO ()
mapOuterEventsToInner fromOuterBus triggerInnerEvent = do
  liftIOLater $ wrapAsync $ do runEffect $ fromInput fromOuterBus >-> fireEventConsumer triggerInnerEvent
                               performGC

fireServerEventsTask :: B.Event E.Event -> Handler E.Event -> MomentIO ()
fireServerEventsTask innerEvent triggerEvent = do
    (serverEvent, x) <- mapAccum Nothing $ scanServerInput <$> filterE isServerInput innerEvent
    reactimate $ triggerEventAction <$> serverEvent
    where isServerInput (ServerInput _) = True
          isServerInput _ = False
          triggerEventAction evt = mapM_ triggerEvent evt

scanServerInput :: E.Event -> Maybe (Result ServerEvent) -> ([E.Event], Maybe (Result ServerEvent))
scanServerInput (ServerInput text) Nothing = parseWholeServerInput (A.parse serverInputParser text) []
scanServerInput (ServerInput text) (Just (Partial cont)) = parseWholeServerInput (cont text) []

parseWholeServerInput :: Result ServerEvent -> [E.Event] -> ([E.Event], Maybe (Result ServerEvent))
parseWholeServerInput (Done "" r) events = ((ServerEvent r):events, Nothing)
parseWholeServerInput (Done remaining evt) events = let nextResult = A.parse serverInputParser remaining
                                                     in parseWholeServerInput nextResult ((ServerEvent evt):events)
parseWholeServerInput cnt@(Partial cont) events = (events, Just cnt)

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
    where isServerCommand (ServerCommand _) = True
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
    (sock, addr) <- NST.connectSock "bylins.su" "4000"
    updateSocketBehavior $ Just sock

    let closeSockOnEof = NST.closeSock sock
        cleanup = liftIO (updateSocketBehavior Nothing) >> closeSockOnEof >> fireDisconnectionEvent

    async $ do runEffect $ (fromSocket sock (2^15)  >> liftIO (sendToConsole toOuterBus "socket channel finished\n") >> cleanup) >-> fireServerInputEventConsumer toOuterBus
               performGC
    return ()
      where fireDisconnectionEvent = liftIO $ wrapAsync $ atomically $ PC.send toOuterBus ServerDisconnection

fireServerInputEventConsumer :: Output E.Event -> Consumer ByteString IO ()
fireServerInputEventConsumer toOuterBus = do text <- await
                                             liftIO $ do async $ do atomically $ PC.send toOuterBus $ ServerInput text
                                                         return ()
                                             fireServerInputEventConsumer toOuterBus

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

{--fireEventsFromServerInput :: Handler ServerEvent -> IO (Output ByteString, STM ())
fireEventsFromServerInput fireServerEvent = do
    return (parseServerTextOut, seal)

mapperTask :: World -> B.Event E.PersonCommand -> SendToConsolesAction -> MomentIO ()
mapperTask world consoleCommandEvent sendToConsolesAction = do
  (bLoc, _) <- newBehavior $ Nothing
  let graph = buildMap $ directions world
      findPathResponse = showFindPathResponse world graph <$> bLoc <@> filterE isFindPath consoleCommandEvent
  reactimate $ (sendToConsolesAction . matchingLocs world) <$> filterE isFindLoc consoleCommandEvent
  reactimate $ sendToConsolesAction <$> findPathResponse

showFindPathResponse :: World -> Gr () Int -> Maybe Int -> E.PersonCommand -> ByteString
showFindPathResponse world graph currLoc userInput =
  let destination = locsByRegex world
      showPathBy f t = if (f == t) then "you are already there!"
                           else (showPath $ directions world) $ GA.sp f t graph
   in case (currLoc, userInput) of
        (_, FindPathFromTo from to) -> showPathBy from to
        (Just currLoc, FindPathToLocId to) -> showPathBy currLoc to
        (Just currLoc, FindPathTo regex) -> let matchingLocs = destination regex
                                             in case S.toList $ matchingLocs of
                                                  [] -> "no matching locations found"
                                                  d:[] -> showPathBy currLoc (locId d)
                                                  _ -> showLocs matchingLocs
        (Nothing, _) -> "current location is unknown\n"

buildMap :: Set Direction -> Gr () Int
buildMap directions = mkGraph nodes edges
  where edges = F.concat $ (\d -> aheadEdge d : reverseEdge d : []) <$> (S.toList directions)
        nodes = fmap (\n -> (n, ())) $ F.foldl (\acc d -> locIdFrom d : locIdTo d : acc) [] directions
        aheadEdge d = (locIdFrom d, locIdTo d, 1)
        reverseEdge d = (locIdTo d, locIdFrom d, 1)

pathFromTo :: Gr () Int -> Maybe Int -> E.PersonCommand -> Maybe Path
pathFromTo graph _ (FindPathFromTo from to) = Just $ GA.sp from to graph
pathFromTo graph (Just currLoc) (FindPathToLocId to) = Just $ GA.sp currLoc to graph
--pathFromTo graph (Just currLoc) (FindPathTo to) =
pathFromTo graph Nothing _ = Nothing

pathTo :: Gr () Int -> Maybe Int -> E.PersonCommand -> Path
pathTo graph (Just from) (FindPathToLocId to) = []

showPath :: Set Direction -> Path -> ByteString
showPath directions [] = "path is empty\n"
showPath directions path = (encodeUtf8 . addRet . joinToOneMsg) (showDirection . nodePairToDirection <$> toJust <$> nodePairs)
  where joinToOneMsg = T.intercalate "\n"
        showDirection = trigger
        addRet txt = T.snoc txt '\n'
        nodePairToDirection (from, to) = P.head $ S.toList $ S.filter (\d -> locIdFrom d == from && locIdTo d == to) directions
        nodePairs = filterDirs $ P.scanl (\acc item -> (snd acc, Just item)) (Nothing, Nothing) path
        filterDirs = P.filter (\pair -> isJust (fst pair) && isJust (snd pair))
        toJust (Just left, Just right) = (left, right)

isFindPath :: E.PersonCommand -> Bool
isFindPath (FindPathFromTo from to) = True
isFindPath (FindPathToLocId to) = True
isFindPath (FindPathTo to) = True
isFindPath _ = False

isFindLoc :: E.PersonCommand -> Bool
isFindLoc (FindLoc regex) = True
isFindLoc _ = False

matchingLocs :: World -> E.PersonCommand -> ByteString
matchingLocs graph (FindLoc regex) = showLocs $ locsByRegex graph regex

writeLog :: IO (Output ByteString, STM ())
writeLog = do
  logFile <- openFile "log" WriteMode
  (persToLogOut, input, seal) <- spawn' $ bounded 1024
  async $ do runEffect $ fromInput input >-> PBS.toHandle logFile >> liftIO (hClose logFile) >> liftIO (DBC8.putStr "write log channel closed\n")
             performGC
  return (persToLogOut, seal)

moveToTask :: B.Event MoveRequest -> B.Event ServerEvent -> MomentIO ()
moveToTask moveRequest serverEvent = do
    let locEvent = filterE isLocation serverEvent
    let moveEvent = filterE isMove serverEvent
    bRequests <- accumB [] $ addRequestEvent moveRequest
    let changeCurrLocEvent = B.unions [(\l@(LocationEvent locData) -> const locData) <$> locEvent
                                     ,(\m@(MoveEvent _ locData) -> const locData) <$> moveEvent]

    reactimate $ moveCommand <$> bRequests <@> changeCurrLocEvent

addRequestEvent :: B.Event MoveRequest -> B.Event ([(TaskKey, Location)] -> [(TaskKey, Location)])
addRequestEvent moveRequest = (\mr@(MoveRequest k dest) acc -> (k, dest) : acc) <$> moveRequest

moveCommand :: [(TaskKey, Location)] -> (Location -> Location) -> IO ()
moveCommand = undefined

isLocation :: ServerEvent -> Bool
isLocation (LocationEvent _) = True
isLocation _ = False

isMove :: ServerEvent -> Bool
isMove (MoveEvent _ _) = True
isMove _ = False

notCommandInput :: E.Event -> Bool
notCommandInput (E.PersonCommand (UserInputRedirect _)) = True
notCommandInput _ = False

sendToConsoleConsumer :: SendToConsolesAction -> Consumer ByteString IO ()
sendToConsoleConsumer action = do
    text <- await
    liftIO $ action text
    sendToConsoleConsumer action

printDirection :: GoDirectionAction -> IO ()
printDirection (GoDirectionAction direction) = DTIO.putStrLn direction

removePathHead :: Maybe [Text] -> Maybe [Text]
removePathHead Nothing = Nothing
removePathHead (Just []) = Nothing
removePathHead x@(Just xs) = fmap P.tail x

bPathNotEmpty :: Behavior (Maybe [Text]) -> Behavior Bool
bPathNotEmpty = fmap pathNotEmpty

pathNotEmpty :: Maybe [Text] -> Bool
pathNotEmpty Nothing = False
pathNotEmpty (Just []) = False
pathNotEmpty (Just xs) = True--}

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

loadWorld :: FilePath -> IO World
loadWorld dir = do
  files <- listDirectory dir
  directions <- loadDirs files
  locations <- loadLocs files
  return $ World locations directions
    where loadDirs files = F.foldl (\acc item -> loadDirections acc (dir ++ item)) (return S.empty) files
          loadLocs files = F.foldl (\acc item -> loadLocations acc (dir ++ item)) (return S.empty) files

parseProducer :: Producer ByteString IO () -> Producer (Maybe (Either ParsingError ServerEvent)) IO ()
parseProducer src = do
    (result, partial) <- liftIO $ runStateT (PA.parse serverInputParser) src
    continue result partial
    where continue result@(Just (Right _)) partial = do yield result
                                                        parseProducer partial
          continue (Just (Left err)) _ = liftIO $ DBC8.putStr "error\n"
          continue Nothing _ = liftIO $ DBC8.putStr "parsed entire stream\n"
