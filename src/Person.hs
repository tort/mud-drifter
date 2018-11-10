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
  where afterTravel locId (Failure err loc) = yield (ConsoleOutput $ "travel to " <> (show locId) <> " failed: " <> err) >> waitTravelRequest (Just loc)
        afterTravel locId (Success loc) = yield (ConsoleOutput $ "travel to " <> (show locId) <> " finished") >> waitTravelRequest (Just loc)
        waitTravelRequest currLoc = await >>= \evt -> case evt of (UserCommand (GoToLocId locId)) -> travelPath currLoc locId >>= afterTravel locId
                                                                  (ServerEvent (LocationEvent (Location locId _) _)) -> yield evt >> waitTravelRequest (Just locId)
                                                                  _ -> yield evt >> waitTravelRequest currLoc
        travelPath (Just currLoc) locId = travel world $ GA.sp currLoc locId (worldMap world)
        travelPath Nothing locId = (return $ Failure "current location is unknown" locId)

type Reason = ByteString
data TravelResult = Success LocId | Failure Reason LocId

travel :: MonadSafe m => World -> Path -> Pipe Event Event m TravelResult
travel world [locId] = return $ Success locId
travel world path = makeStep
  where chopPath locId path = L.dropWhile (/=locId) path :: Path
        go (from:to:xs) = do findMoveQuests world from to
                             mapM_ (yield . SendToServer) (trigger <$> (findDirection (directions world) from to))
        go [_] = return ()
        go [] = return ()
        makeStep = await >>= \evt -> case evt of PulseEvent -> go path >> waitMove
                                                 e -> yield e >> makeStep
        waitMove = await >>= handleLocationEvent
        handleLocationEvent evt@(ServerEvent (LocationEvent (Location locId _) _)) = yield evt >> if L.elem locId path
                                                                                                     then travel world (chopPath locId path)
                                                                                                     else return $ Failure "path lost" locId
        handleLocationEvent PulseEvent = waitMove
        handleLocationEvent e = yield e >> waitMove

findMoveQuests :: MonadSafe m => World -> LocId -> LocId -> Pipe Event Event m ()
findMoveQuests world from to = action $ M.lookup (from, to) ta
  where action (Just $ OpenDoor dir) = do yield $ SendToServer ("смотр " <> show dir)
                                          await >>= \evt -> case evt of (GlanceObstacle _ obstacle) -> yield $ SendToServer "открыть " <> obstacle <> " " <> dir
                                                                        _ -> return ()
        action _ = return ()
        ta = travelActions world :: Map (LocId, LocId) TravelActionType

findDirection :: Set Direction -> LocId -> LocId -> Maybe Direction
findDirection directions from to = L.find (\d -> locIdFrom d == from && locIdTo d == to) directions

loadConfigProperty :: Text -> IO (Maybe Text)
loadConfigProperty propertyName = do conf <- DC.load [Required personCfgFileName]
                                     propertyValue <- DC.lookup conf propertyName
                                     return propertyValue

personCfgFileName :: String
personCfgFileName = "person.cfg"
