{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Drifter ( drifter
               , parseServerInputPipe
               , login
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

drifter :: Monad m => World -> Pipe Event Event m ()
drifter world = parseUserInputPipe >-> parseServerInputPipe >-> mapper world >-> person world

login :: Pipe Event Event IO ()
login = await >>= \case (ServerEvent CodepagePrompt) -> yield (SendToServer "5") >> login
                        (ServerEvent LoginPrompt) -> yield (SendToServer "генод") >> login
                        (ServerEvent PasswordPrompt) -> yield (SendToServer "каркасный") >> login
                        (ServerEvent WelcomePrompt) -> yield (SendToServer "")
                        _ -> login

parseUserInputPipe :: Monad m => Pipe Event Event m ()
parseUserInputPipe = PP.map parse
  where parse (ConsoleInput text) = handleCmd $ parseUserInput text
        parse x = x
        handleCmd (Right (ServerCommand text)) = SendToServer text
        handleCmd (Right cmd) = UserCommand cmd
        handleCmd (Left err) = ConsoleOutput $ pack $ show err

parseServerInputPipe :: Monad m => Pipe Event Event m ()
parseServerInputPipe = parseWithState Nothing
  where parseWithState state = do evt <- await
                                  case evt of input@(ServerInput _) -> f $ scanServerInput input state
                                              x -> yield x >> parseWithState state
        f (serverEvents, state) = do F.mapM_ yield serverEvents
                                     parseWithState state

onerr :: Monad m => SomeException -> Pipe Event Event m ()
onerr ex = yield (ConsoleOutput "error!!")


scanServerInput :: Event -> Maybe (Result ServerEvent) -> ([Event], Maybe (Result ServerEvent))
scanServerInput (ServerInput text) Nothing = parseWholeServerInput (A.parse serverInputParser text) []
scanServerInput (ServerInput "") _ = ([], Nothing)
scanServerInput (ServerInput text) (Just (Partial cont)) = parseWholeServerInput (cont text) []

parseWholeServerInput :: Result ServerEvent -> [Event] -> ([Event], Maybe (Result ServerEvent))
parseWholeServerInput (Done "" r) events = (events ++ [ServerEvent r], Nothing)
parseWholeServerInput (Done remaining evt) events = let nextResult = A.parse serverInputParser remaining
                                                     in parseWholeServerInput nextResult (events ++ [ServerEvent evt])
parseWholeServerInput cnt@(Partial cont) events = (events, Just cnt)
parseWholeServerInput (Fail remaining contexts desc) events = (events ++ [errorEvt contexts desc], Nothing)
  where errorEvt contexts desc = ConsoleOutput ("parsing error: " <> pack desc <> " contexts: " <> (unwords $ pack <$> contexts))

