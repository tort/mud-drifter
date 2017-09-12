{-# LANGUAGE OverloadedStrings #-}

module UserInputParser (userInputParser
                       ) where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Error
import Data.Text
import Text.Read
import Event

userInputParser :: Parser PersonCommand
userInputParser = nonCommandInputParser
                  <|> try connParser
                  <|> try findLocParser
                  <|> try unconnParser
                  <|> try emptyInputParser
                  <|> try findPathToLocIdParser
                  <|> try findPathFromToParser
                  <|> try findPathToLocParser
                  <|> try goToParser
                  <|> unknownCommandParser

nonCommandInputParser :: Parser PersonCommand
nonCommandInputParser = do
  c <- noneOf "/"
  input <- many anyChar
  return $ UserInputRedirect $ pack (c : input)

findLocParser :: Parser PersonCommand
findLocParser = do
  string "/findloc"
  optional space
  input <- many anyChar
  return $ FindLoc $ pack input

connParser :: Parser PersonCommand
connParser = do
  string "/conn"
  return $ KeepConn True

unconnParser :: Parser PersonCommand
unconnParser = do
  string "/unconn"
  return $ KeepConn False

unknownCommandParser :: Parser PersonCommand
unknownCommandParser = do
  lookAhead $ char '/'
  input <- many anyChar
  unexpected $ "command " ++ input ++ "\n"

emptyInputParser :: Parser PersonCommand
emptyInputParser = do
  eof
  return $ UserInputRedirect ""

findPathFromToParser :: Parser PersonCommand
findPathFromToParser = do
  string "/path"
  many1 space
  from <- optionMaybe (many1 digit)
  many1 space
  to <- optionMaybe (many1 digit)
  many space
  case FindPathFromTo <$> (readMaybe =<< from) <*> (readMaybe =<< to) of
    Nothing -> unexpected " /path format\n"
    Just cmd -> return cmd

findPathToLocIdParser :: Parser PersonCommand
findPathToLocIdParser = do
  string "/path"
  many1 space
  to <- many1 digit
  many space
  eof
  case FindPathToLocId <$> (readMaybe to) of
    Nothing -> unexpected " /path format\n"
    Just cmd -> return cmd

findPathToLocParser :: Parser PersonCommand
findPathToLocParser = do
  string "/path"
  many1 space
  to <- many1 anyChar
  eof
  return $ FindPathTo $ stripEnd $ pack to

goToParser :: Parser PersonCommand
goToParser = do
  string "/go"
  many1 space
  to <- many1 anyChar
  eof
  let strippedTo = stripEnd $ pack to
  case readMaybe (unpack strippedTo) of
    Nothing -> return $ GoTo $ strippedTo
    Just locId -> return $ GoToLocId locId
