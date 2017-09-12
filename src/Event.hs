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

data Event = ConsoleInput Text
           | PersonCommand PersonCommand
           | ServerCommand Text
           | ServerInput ByteString
           | ServerEvent ServerEvent
           | ConsoleOutput ByteString
           | ServerDisconnection
           deriving (Eq, Show)

type EventBus = (Output Event, Input Event)

data PersonCommand = UserInputRedirect Text
               | KeepConn Bool
               | FindLoc Text
               | FindPathFromTo Int Int
               | FindPathToLocId Int
               | FindPathTo Text
               | GoTo Text
               | GoToLocId Int
               deriving (Eq, Show)

data ServerEvent = CodepagePrompt | LoginPrompt | PasswordPrompt | WelcomePrompt | PostWelcome | LocationEvent Location | MoveEvent Text Location | UnknownServerEvent deriving (Eq, Show)


data Location = Location { locId :: LocId
                         , locTitle :: LocTitle
                         } deriving (Show, Ord)
type LocId = Int
type LocTitle = Text

instance Eq Location where
  left == right = locId left == locId right
