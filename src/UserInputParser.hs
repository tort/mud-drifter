{-# LANGUAGE OverloadedStrings #-}

module UserInputParser (userInputParser
                       , UserInput(..)
                       ) where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Error
import Data.Text
import Text.Read

data UserInput = UserInput Text | KeepConn Bool | FindLoc Text | FindPathFromTo Int Int deriving (Eq, Show)

userInputParser :: Parser UserInput
userInputParser = nonCommandInputParser
                  <|> try connParser
                  <|> try findLocParser
                  <|> try unconnParser
                  <|> try emptyInputParser
                  <|> try findPathFromToParser
                  <|> unknownCommandParser

nonCommandInputParser :: Parser UserInput
nonCommandInputParser = do
  c <- noneOf "/"
  input <- many anyChar
  return $ UserInput $ pack (c : input)

findLocParser :: Parser UserInput
findLocParser = do
  string "/findloc"
  optional space
  input <- many anyChar
  return $ FindLoc $ pack input

connParser :: Parser UserInput
connParser = do
  string "/conn"
  return $ KeepConn True

unconnParser :: Parser UserInput
unconnParser = do
  string "/unconn"
  return $ KeepConn False

unknownCommandParser :: Parser UserInput
unknownCommandParser = do
  lookAhead $ char '/'
  input <- many anyChar
  unexpected $ "command " ++ input ++ "\n"

emptyInputParser :: Parser UserInput
emptyInputParser = do
  eof
  return $ UserInput ""

findPathFromToParser :: Parser UserInput
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
