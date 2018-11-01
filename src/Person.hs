{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Person ( person
              , loadWorld
              ) where

import Protolude hiding ((<>), Location, runStateT)
import qualified Pipes.Prelude as PP
import Data.Monoid
import Data.Char
import Data.Text
import Data.List
import qualified Data.List as L
import qualified Data.Text as T
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

person :: MonadSafe m => Pipe Event Event m ()
person = PP.map identity

loadConfigProperty :: Text -> IO (Maybe Text)
loadConfigProperty propertyName = do conf <- DC.load [Required personCfgFileName]
                                     propertyValue <- DC.lookup conf propertyName
                                     return propertyValue

personCfgFileName :: String
personCfgFileName = "person.cfg"
