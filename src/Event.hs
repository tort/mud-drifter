{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Event ( Event(..)
             , PersonCommand(..)
             , ServerEvent(..)
             , EventBus
             , Location(..)
             , LocId
             , LocTitle
             ) where

import Pipes.Concurrent
import Data.Text
import Data.ByteString

import Data.Binary
import GHC.Generics (Generic)

instance Binary Event
instance Binary PersonCommand
instance Binary ServerEvent
instance Binary Location

data Event = ConsoleInput Text
           | PersonCommand PersonCommand
           | ServerCommand Text
           | ServerInput ByteString
           | ServerEvent ServerEvent
           | ConsoleOutput ByteString
           | ServerDisconnection
           deriving (Eq, Show, Generic)

type EventBus = (Output Event, Input Event)

data PersonCommand = UserInputRedirect Text
               | KeepConn Bool
               | FindLoc Text
               | FindPathFromTo Int Int
               | FindPathToLocId Int
               | FindPathTo Text
               | GoTo Text
               | GoToLocId Int
               deriving (Eq, Show, Generic)

data ServerEvent = CodepagePrompt | LoginPrompt | PasswordPrompt | WelcomePrompt | PostWelcome | LocationEvent Location | MoveEvent Text | DarknessEvent |  UnknownServerEvent ByteString deriving (Eq, Show, Generic)


data Location = Location { locId :: LocId
                         , locTitle :: LocTitle
                         } deriving (Show, Ord, Generic)
type LocId = Int
type LocTitle = Text

instance Eq Location where
  left == right = locId left == locId right
