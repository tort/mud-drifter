{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Person ( person
              , loadWorld
              ) where

import Protolude hiding ((<>), Location, runStateT)
import qualified Pipes.Prelude as PP
import Data.Monoid
import Data.Char
import Data.List
import qualified Data.List as L
import Pipes
import Pipes.Concurrent
import qualified Pipes.Concurrent as PC
import System.IO (hClose, openFile, Handle, withFile, IOMode(..))
import Data.ByteString.Char8 as DBC8 hiding (snoc)
import ServerInputParser
import Data.Attoparsec.ByteString as A
import Pipes.Attoparsec
import qualified Pipes.Attoparsec as PA
import Pipes.Parse
import Pipes.Safe
import qualified Data.Configurator as DC
import Data.Configurator.Types
import Mapper
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.Graph as G
import qualified Pipes.ByteString as PBS
import qualified Data.Foldable as F
import System.Directory
import UserInputParser
import qualified Data.Graph.Inductive.Query.SP as GA
import Data.String
import qualified Data.Set as S
import Event
import Control.Concurrent.Timer
import qualified Data.Map.Strict as M
import Logger
import World
import Data.Maybe

person :: MonadSafe m => World -> Pipe Event Event m ()
person world = travelTask world

travelTask :: MonadSafe m => World -> Pipe Event Event m ()
travelTask world = waitTravelRequest Nothing
  where afterTravel locId (Failure err loc) = yield (ConsoleOutput $ "travel to " <> (show locId) <> " failed: " <> err) >> waitTravelRequest loc
        afterTravel locId (Success loc) = yield (ConsoleOutput $ "travel to " <> (show locId) <> " finished") >> waitTravelRequest (Just loc)
        waitTravelRequest currLoc = await >>= \evt -> case evt of (UserCommand (GoToLocId locId)) -> travelPath currLoc locId >>= afterTravel locId
                                                                  (ServerEvent (LocationEvent (Location locId _) _)) -> yield evt >> waitTravelRequest (Just locId)
                                                                  (ServerEvent DarknessEvent) -> yield evt >> waitTravelRequest Nothing
                                                                  _ -> yield evt >> waitTravelRequest currLoc
        travelPath (Just currLoc) locId = travel world $ GA.sp currLoc locId (worldMap world)
        travelPath Nothing locId = (return $ Failure "current location is unknown" Nothing)

type Reason = ByteString
data TravelResult = Success LocId | Failure Reason (Maybe LocId)

travel :: MonadSafe m => World -> Path -> Pipe Event Event m TravelResult
travel world [locId] = return $ Success locId
travel world path = makeStep
  where chopPath locId path = L.dropWhile (/=locId) path :: Path
        go (from:to:xs) = do findMoveQuests from to
                             mapM_ (yield . SendToServer) (trigger <$> (findDirection (directions world) from to))
        go [_] = return ()
        go [] = return ()
        makeStep = await >>= \evt -> case evt of PulseEvent -> go path >> waitMove
                                                 e -> yield e >> makeStep
        waitMove = await >>= handleLocationEvent
        handleLocationEvent evt@(ServerEvent (LocationEvent (Location locId _) _)) = yield evt >> if L.elem locId path
                                                                                                     then travel world (chopPath locId path)
                                                                                                     else return $ Failure "path lost" (Just locId)
        handleLocationEvent evt@(ServerEvent (MoveEvent dir)) = let from:to:xs = path
                                                                 in yield evt >> case ((== dir) <$> trigger <$> (findDirection (directions world) from to))
                                                                                      of Nothing -> waitMove
                                                                                         (Just False) -> return $ Failure "path lost" Nothing
                                                                                         (Just True) -> waitMove
        handleLocationEvent evt@(ServerEvent CantGoDir) = yield evt >> (return $ Failure "path lost" Nothing)
        handleLocationEvent evt@(ServerEvent DarknessEvent) = yield evt >> travel world (L.tail path)
        handleLocationEvent PulseEvent = waitMove
        handleLocationEvent e = yield e >> waitMove

findMoveQuests :: MonadSafe m => LocId -> LocId -> Pipe Event Event m ()
findMoveQuests from to = action $ L.lookup (from, to) travelActions
  where action (Just quest) = quest
        action _ = return ()

findDirection :: Set Direction -> LocId -> LocId -> Maybe Direction
findDirection directions from to = L.find (\d -> locIdFrom d == from && locIdTo d == to) directions

loadConfigProperty :: Text -> IO (Maybe Text)
loadConfigProperty propertyName = do conf <- DC.load [Required personCfgFileName]
                                     propertyValue <- DC.lookup conf propertyName
                                     return propertyValue

personCfgFileName :: String
personCfgFileName = "person.cfg"

travelActions :: MonadSafe m => [((LocId, LocId), (Pipe Event Event m ()))]
travelActions = [ ((5102, 5103), openDoor South)
                , ((5104, 5117), setupLadder)
                ]

setupLadder :: MonadSafe m => Pipe Event Event m ()
setupLadder = yield $ SendToServer "приставить лестница"

openDoor :: MonadSafe m => RoomDir -> Pipe Event Event m ()
openDoor dir = do yield $ SendToServer ("смотр " <> show dir)
                  await >>= yield
                  await >>= \evt -> case evt of (ServerEvent (ObstacleEvent _ obstacle)) -> yield evt >> (yield $ SendToServer ("открыть " <> obstacle <> " " <> show dir))
