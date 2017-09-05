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
userInputParser = do
  input <- many anyChar
  return $ UserInput $ pack input
