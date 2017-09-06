{-# LANGUAGE OverloadedStrings #-}

module Person ( runPerson
              , parseProducer -- TODO move to separate module
              , SendToConsolesAction
              , loadWorld
              , mapperTask
              ) where

import Control.Applicative ((<|>))
import Data.Monoid
import Data.Char
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as DTIO
import Reactive.Banana
import qualified Reactive.Banana as B
import Reactive.Banana.Frameworks
import Data.Maybe
import qualified Network.Simple.TCP as NST
import qualified Network.Socket.ByteString as NBS
import qualified Network.Socket as NS
import Pipes
import Pipes.Concurrent
import Pipes.Network.TCP as PNT
import System.IO (hClose, openFile, Handle, withFile, IOMode(..))
import Data.Text.Encoding
import Data.ByteString.Char8 as DBC8 hiding (isInfixOf, isPrefixOf, snoc, putStrLn)
import Control.Concurrent.Async
import ServerInputParser
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

newtype GoDirectionAction = GoDirectionAction Text
data MoveRequest = MoveRequest TaskKey Location
type TaskKey = Int
type SendToConsolesAction = ByteString -> IO ()

newtype KeepConnectionCommand = KeepConnectionCommand Bool
data DisconnectEvent = DisconnectEvent
data ServerCommand = ServerCommand Text

runPerson :: World -> SendToConsolesAction -> IO (Output Text)
runPerson world sendToConsolesAction = do
    personBox <- spawn $ bounded 1024
    network <- compile $ personTask world (snd personBox) sendToConsolesAction
    actuate network
    return $ fst personBox

personTask :: World -> Input Text -> SendToConsolesAction -> MomentIO ()
personTask world fromConsoles sendToConsolesAction = do
  consoleCommandEvent <- fireEventsFromConsolesInput sendToConsolesAction fromConsoles
  mapperTask world consoleCommandEvent sendToConsolesAction
  keepLoggedTask consoleCommandEvent sendToConsolesAction

mapperTask :: World -> Event UserInput -> SendToConsolesAction -> MomentIO ()
mapperTask world consoleCommandEvent sendToConsolesAction = do
  let graph = buildMap $ directions world
  reactimate $ (sendToConsolesAction . matchingLocs world) <$> filterE isFindLoc consoleCommandEvent
  reactimate $ (sendToConsolesAction . (showPath $ directions world) . pathFromTo graph) <$> filterE isFindPath consoleCommandEvent

buildMap :: Set Direction -> Gr () Int
buildMap directions = mkGraph nodes edges
  where edges = F.concat $ (\d -> aheadEdge d : reverseEdge d : []) <$> (S.toList directions)
        nodes = fmap (\n -> (n, ())) $ F.foldl (\acc d -> locIdFrom d : locIdTo d : acc) [] directions
        aheadEdge d = (locIdFrom d, locIdTo d, 1)
        reverseEdge d = (locIdTo d, locIdFrom d, 1)

pathFromTo :: Gr () Int -> UserInput -> Path
pathFromTo graph (FindPathFromTo from to) = GA.sp from to graph

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

isFindPath :: UserInput -> Bool
isFindPath (FindPathFromTo from to) = True
isFindPath _ = False

isFindLoc :: UserInput -> Bool
isFindLoc (FindLoc regex) = True
isFindLoc _ = False

matchingLocs :: World -> UserInput -> ByteString
matchingLocs graph (FindLoc regex) = locsByRegex graph regex

sendServerCommandTask :: Behavior (Maybe Socket) -> SendToConsolesAction -> MomentIO (Handler ServerCommand)
sendServerCommandTask bSocket sendToConsolesAction = do
  (serverCommandEvent, fireServerCommand) <- newEvent
  let txt = (\(ServerCommand txt) -> txt) <$> serverCommandEvent
  reactimate $ sendCommand sendToConsolesAction <$> bSocket <@> txt
  return fireServerCommand

writeLog :: IO (Output ByteString, STM ())
writeLog = do
  logFile <- openFile "log" WriteMode
  (persToLogOut, input, seal) <- spawn' $ bounded 1024
  async $ do runEffect $ fromInput input >-> PBS.toHandle logFile >> liftIO (hClose logFile) >> liftIO (DBC8.putStr "write log channel closed\n")
             performGC
  return (persToLogOut, seal)

fireEventsFromConsolesInput :: SendToConsolesAction -> Input Text -> MomentIO (Event UserInput)
fireEventsFromConsolesInput sendToConsolesAction fromConsolesChan = do
    (consoleCommandEvent, trigger) <- newEvent
    liftIOLater $ do async $ runEffect $ fromInput fromConsolesChan >-> handleInputFromConsolesConsumer sendToConsolesAction trigger
                     return ()
    return consoleCommandEvent

moveToTask :: Event MoveRequest -> Event ServerEvent -> MomentIO ()
moveToTask moveRequest serverEvent = do
    let locEvent = filterE isLocation serverEvent
    let moveEvent = filterE isMove serverEvent
    bRequests <- accumB [] $ addRequestEvent moveRequest
    let changeCurrLocEvent = B.unions [(\l@(LocationEvent locData) -> const locData) <$> locEvent
                                     ,(\m@(MoveEvent _ locData) -> const locData) <$> moveEvent]

    reactimate $ moveCommand <$> bRequests <@> changeCurrLocEvent

addRequestEvent :: Event MoveRequest -> Event ([(TaskKey, Location)] -> [(TaskKey, Location)])
addRequestEvent moveRequest = (\mr@(MoveRequest k dest) acc -> (k, dest) : acc) <$> moveRequest

moveCommand :: [(TaskKey, Location)] -> (Location -> Location) -> IO ()
moveCommand = undefined

isLocation :: ServerEvent -> Bool
isLocation (LocationEvent _) = True
isLocation _ = False

isMove :: ServerEvent -> Bool
isMove (MoveEvent _ _) = True
isMove _ = False

handleInputFromConsolesConsumer :: SendToConsolesAction -> Handler UserInput -> Consumer Text IO ()
handleInputFromConsolesConsumer sendToConsolesAction fireConsoleCommandEvent = do
  text <- await
  liftIO $ case Parsec.parse userInputParser "" text of (Right cmd) -> fireConsoleCommandEvent cmd
                                                        (Left err) -> sendToConsolesAction $ DBC8.pack $ show err
  handleInputFromConsolesConsumer sendToConsolesAction fireConsoleCommandEvent

keepConnectedTask :: Event UserInput -> SendToConsolesAction -> Handler ServerEvent -> MomentIO (Handler ServerCommand)
keepConnectedTask consoleCommandEvent sendToConsolesAction fireServerEvent = do
    (disconnectionEvent, fireDisconnection) <- newEvent
    let keepConnectionEvent = keepConnectionCommandToEvent <$> filterE isKeepConnectionCommand consoleCommandEvent
    bKeepConnection <- stepper False keepConnectionEvent
    (bSocket, fireUpdateSocket) <- newBehavior Nothing
    sendServerCommand <- sendServerCommandTask bSocket sendToConsolesAction

    reactimate $ connectToServer fireDisconnection fireUpdateSocket sendToConsolesAction fireServerEvent <$ whenE (isNothing <$> bSocket) (filterE id keepConnectionEvent)
    reactimate $ connectToServer fireDisconnection fireUpdateSocket sendToConsolesAction fireServerEvent <$ whenE bKeepConnection disconnectionEvent
    reactimate $ sendServerCommand <$> ((\(UserInput txt) -> ServerCommand txt) <$> filterE notCommandInput consoleCommandEvent)
    return sendServerCommand

keepLoggedTask :: Event UserInput -> SendToConsolesAction -> MomentIO ()
keepLoggedTask fromConsoles sendToConsolesAction = do
    (serverEvent, fireServerEvent) <- newEvent
    sendServerCommand <- keepConnectedTask fromConsoles sendToConsolesAction fireServerEvent

    reactimate $ sendServerCommand codepageServerCommand <$ filterE (== CodepagePrompt) serverEvent
    reactimate $ loadLoginAndSendCommand sendServerCommand <$ filterE (== LoginPrompt) serverEvent
    reactimate $ loadPasswordAndSendCommand sendServerCommand <$ filterE (== PasswordPrompt) serverEvent
    reactimate $ sendServerCommand emptyServerCommand <$ filterE (== WelcomePrompt) serverEvent
      where loadLoginAndSendCommand sendServerCommand = handle =<< loadConfigProperty "person.login"
                                                             where handle (Just login) = sendServerCommand $ ServerCommand login
                                                                   handle Nothing = sendToConsolesAction "failed to load person login\n"
            loadPasswordAndSendCommand sendServerCommand = handle =<< loadConfigProperty "person.password"
                                                                where handle (Just pass) = sendServerCommand $ ServerCommand pass
                                                                      handle Nothing = sendToConsolesAction "failed to load person password\n"

personCfgFileName :: String
personCfgFileName = "person.cfg"

codepageServerCommand :: ServerCommand
codepageServerCommand = ServerCommand "5"

loadConfigProperty :: Text -> IO (Maybe Text)
loadConfigProperty propertyName = do conf <- DC.load [Required personCfgFileName]
                                     propertyValue <- DC.lookup conf propertyName
                                     return propertyValue

emptyServerCommand :: ServerCommand
emptyServerCommand = ServerCommand ""

sendCommand :: SendToConsolesAction -> Maybe Socket -> Text -> IO ()
sendCommand _ (Just sock) txt = NST.send sock $ encodeUtf8 $ snoc txt '\n'
sendCommand sendToConsolesAction Nothing _ = sendToConsolesAction "no connection to server\n"

keepConnectionCommandToEvent :: UserInput -> Bool
keepConnectionCommandToEvent (KeepConn v) = v

isKeepConnectionCommand :: UserInput -> Bool
isKeepConnectionCommand (KeepConn _) = True
isKeepConnectionCommand _ = False

notCommandInput :: UserInput -> Bool
notCommandInput (UserInput _) = True
notCommandInput _ = False

connectToServer :: Handler DisconnectEvent -> Handler (Maybe Socket) -> SendToConsolesAction -> Handler ServerEvent -> IO ()
connectToServer fireDisconnection updateSocketBehavior sendToConsolesAction fireServerEvent = do
    (sock, addr) <- NST.connectSock "bylins.su" "4000"
    updateSocketBehavior $ Just sock

    let closeSockOnEof = NST.closeSock sock
    let fireDisconnectionEvent = liftIO $ fireDisconnection DisconnectEvent

    (writeLogChan, sealLogChain) <- writeLog
    (parseServerInputChan, sealServerInputParser) <- fireEventsFromServerInput fireServerEvent
    let cleanup = liftIO (updateSocketBehavior Nothing) >> liftIO (atomically sealLogChain) >> liftIO (atomically sealServerInputParser)
    (toCOut, toCIn) <- spawn $ bounded 1024
    async $ runEffect $ fromInput toCIn >-> sendToConsoleConsumer sendToConsolesAction
    async $ do runEffect $ (fromSocket sock (2^15) >> liftIO (sendToConsolesAction "socket channel finished\n") >> closeSockOnEof >> cleanup ) >-> toOutput (toCOut <> parseServerInputChan <> writeLogChan) >> fireDisconnectionEvent
               performGC
    return ()

fireEventsFromServerInput :: Handler ServerEvent -> IO (Output ByteString, STM ())
fireEventsFromServerInput fireServerEvent = do
    (parseServerTextOut, parseServerTextIn, seal) <- spawn' $ bounded 1024
    async $ do runEffect $ parseProducer (fromInput parseServerTextIn) >-> fireServerEventConsumer fireServerEvent
               performGC
    return (parseServerTextOut, seal)

parseProducer :: Producer ByteString IO () -> Producer (Maybe (Either ParsingError ServerEvent)) IO ()
parseProducer src = do
    (result, partial) <- liftIO $ runStateT (PA.parse serverInputParser) src
    continue result partial
    where continue result@(Just (Right _)) partial = do yield result
                                                        parseProducer partial
          continue (Just (Left err)) _ = liftIO $ DBC8.putStr "error\n"
          continue Nothing _ = liftIO $ DBC8.putStr "parsed entire stream\n"

fireServerEventConsumer :: Handler ServerEvent -> Consumer (Maybe (Either ParsingError ServerEvent)) IO ()
fireServerEventConsumer fireServerEvent = do
    event <- await
    liftIO $ handleEvent event
    fireServerEventConsumer fireServerEvent
    where handleEvent Nothing = return ()
          handleEvent (Just (Left err)) = return ()
          handleEvent (Just (Right event)) = fireServerEvent event

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
pathNotEmpty (Just xs) = True

loadWorld :: FilePath -> IO World
loadWorld dir = do
  files <- listDirectory dir
  directions <- loadDirs files
  locations <- loadLocs files
  return $ World locations directions
    where loadDirs files = F.foldl (\acc item -> loadDirections acc (dir ++ item)) (return S.empty) files
          loadLocs files = F.foldl (\acc item -> loadLocations acc (dir ++ item)) (return S.empty) files

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
