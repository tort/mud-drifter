{-# LANGUAGE OverloadedStrings #-}

module Parser (
  serverInputParser
  , ServerEvent(..)
) where

import Control.Applicative
import Pipes.Attoparsec
import qualified Data.Attoparsec.ByteString as A
import Data.Text
import Data.Attoparsec.ByteString
import Data.Text.Encoding
import qualified Data.ByteString as B
import Data.Word

data ServerEvent = CodepagePrompt | LoginPrompt | PasswordPrompt | Raw Text deriving Eq

serverInputParser :: A.Parser ServerEvent
serverInputParser = codepagePrompt <|> loginPrompt <|> passwordPrompt <|> unknownMessage

codepagePrompt :: A.Parser ServerEvent
codepagePrompt = do
    iacWill
    iacWill
    iacWill
    cr
    newline
    string "Using keytable"
    _ <- manyTill (skip (\x -> True)) (string "Select one : ")
    return CodepagePrompt
    where iacWill = do A.word8 255
                       A.word8 251
                       A.anyWord8

loginPrompt :: A.Parser ServerEvent
loginPrompt = do
    string "    "
    cr
    newline
    string " --------"
    _ <- manyTill (skip (\x -> True)) (string $ encodeUtf8 "Введите имя персонажа (или \"новый\" для создания нового): ")
    return LoginPrompt

passwordPrompt :: A.Parser ServerEvent
passwordPrompt = do
    string $ encodeUtf8 "Персонаж с таким именем уже существует. Введите пароль : "
    iacGA
    return PasswordPrompt

unknownMessage :: A.Parser ServerEvent
unknownMessage = do
    text <- manyTill A.anyWord8 iacGA
    return $ Raw $ decodeUtf8 $ B.pack text

iacGA :: A.Parser Word8
iacGA = do A.word8 255
           A.word8 249

cr :: A.Parser Word8
cr = A.word8 13

newline :: A.Parser Word8
newline = A.word8 10
