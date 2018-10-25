{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Drifter ( drifter
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

drifter :: Pipe Event Event IO ()
drifter = parseUserInputPipe >-> mapper >-> person

parseUserInputPipe :: Pipe Event Event IO ()
parseUserInputPipe = PP.map parse
  where parse (ConsoleInput text) = handleCmd $ parseUserInput text
        parse x = x
        handleCmd (Right (ServerCommand text)) = SendToServer text
        handleCmd (Right cmd) = UserCommand cmd
        handleCmd (Left err) = ConsoleOutput $ pack $ show err

parseServerInputPipe :: Pipe Event Event IO ()
parseServerInputPipe = parseWithState Nothing
  where parseWithState state = do evt <- await
                                  yield evt
                                  case evt of input@(ServerInput _) -> let (serverEvents, newState) = scanServerInput input state
                                                                        in do mapM_ yield serverEvents
                                                                              parseWithState newState
                                              x -> parseWithState state

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

