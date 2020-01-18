{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Drifter ( login
               ) where

import Protolude
import Pipes
import qualified Pipes.Prelude as PP
import Event
import Data.ByteString.Char8
import Data.Text hiding (pack, unwords)
import UserInputParser
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as A
import Person
import Mapper
import ServerInputParser
import qualified Data.Foldable as F
import World

login :: Pipe Event Event IO ()
login = await >>= \case (ServerEvent CodepagePrompt) -> yield (SendToServer "5") >> login
                        (ServerEvent LoginPrompt) -> yield (SendToServer "генод") >> login
                        (ServerEvent PasswordPrompt) -> yield (SendToServer "каркасный") >> login
                        (ServerEvent WelcomePrompt) -> yield (SendToServer "")
                        _ -> login

onerr :: Monad m => SomeException -> Pipe Event Event m ()
onerr ex = yield (ConsoleOutput "error!!")
