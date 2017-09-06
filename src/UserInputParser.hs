{-# LANGUAGE OverloadedStrings #-}

module UserInputParser (userInputParser
                       , UserInput(..)
                       ) where

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Error
import Data.Text

data UserInput = UserInput Text | KeepConn Bool | FindLoc Text | FindPath Int deriving (Eq, Show)

userInputParser :: Parser UserInput
userInputParser = nonCommandInputParser
                  <|> try connParser
                  <|> try findLocParser
                  <|> try unconnParser
                  <|> try emptyInputParser
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
