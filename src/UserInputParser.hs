
{-# LANGUAGE OverloadedStrings #-}

module UserInputParser (userInputParser
                       , parseUserInput
                       ) where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Error
import Data.Text
import Text.Read
import Event

parseUserInput :: Text -> Either ParseError UserCommand
parseUserInput text = parse userInputParser "" text

userInputParser :: Parser UserCommand
userInputParser = nonCommandInputParser
                  <|> try connParser
                  <|> try zapParser
                  <|> try findLocParser
                  <|> try emptyInputParser
                  <|> try findPathToLocIdParser
                  <|> try findPathFromToParser
                  <|> try findPathToLocParser
                  <|> try goToParser
                  <|> try equipCmdParser
                  <|> unknownCommandParser

nonCommandInputParser :: Parser UserCommand
nonCommandInputParser = do
  c <- noneOf "/"
  input <- many anyChar
  return $ ServerCommand $ pack (c : input)

findLocParser :: Parser UserCommand
findLocParser = do
  string $ "/лок"
  optional space
  input <- many anyChar
  return $ FindLoc $ pack input

connParser :: Parser UserCommand
connParser = do
  string "/conn"
  return $ Connect

zapParser :: Parser UserCommand
zapParser = do
  string "/zap"
  return $ Zap

unknownCommandParser :: Parser UserCommand
unknownCommandParser = do
  lookAhead $ char '/'
  input <- many anyChar
  unexpected $ "command " ++ input ++ "\n"

emptyInputParser :: Parser UserCommand
emptyInputParser = do
  eof
  return $ ServerCommand ""

findPathFromToParser :: Parser UserCommand
findPathFromToParser = do
  string "/путь"
  many1 space
  from <- optionMaybe (many1 digit)
  many1 space
  to <- optionMaybe (many1 digit)
  many space
  let readLocId maybeTextId = LocationId <$> (readMaybe =<< maybeTextId)
   in case FindPathFromTo <$> (readLocId from) <*> (readLocId to) of
        Nothing -> unexpected " /го format\n"
        Just cmd -> return cmd

findPathToLocIdParser :: Parser UserCommand
findPathToLocIdParser = do
  string "/путь"
  many1 space
  to <- many1 digit
  many space
  eof
  case (FindPathToLocId . LocationId) <$> (readMaybe to) of
    Nothing -> unexpected " /path format\n"
    Just cmd -> return cmd

findPathToLocParser :: Parser UserCommand
findPathToLocParser = do
  string "/путь"
  many1 space
  to <- many1 anyChar
  eof
  return $ FindPathTo $ stripEnd $ pack to

goToParser :: Parser UserCommand
goToParser = do
  string "/го"
  many1 space
  to <- many1 anyChar
  eof
  let strippedTo = stripEnd $ pack to
  case readMaybe (unpack strippedTo) of
    Nothing -> return $ GoTo $ strippedTo
    Just locId -> return $ GoToLocId $ LocationId locId

equipCmdParser :: Parser UserCommand
equipCmdParser = do string "/экип"
                    return Equip
