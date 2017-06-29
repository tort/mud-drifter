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

data ServerEvent = CodepagePrompt | LoginPrompt | PasswordPrompt | WelcomePrompt | PostWelcome | UnknownServerEvent deriving (Eq, Show)

serverInputParser :: A.Parser ServerEvent
serverInputParser = codepagePrompt <|> loginPrompt <|> passwordPrompt <|> welcomePrompt <|> postWelcome <|> unknownMessage

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

welcomePrompt :: A.Parser ServerEvent
welcomePrompt = do
    crnl
    crnl
    string $ encodeUtf8 "Последний раз вы заходили к нам в"
    endsIACGA
    return WelcomePrompt

postWelcome :: A.Parser ServerEvent
postWelcome = do
    crnl
    string $ encodeUtf8 "  Добро пожаловать на землю Киевскую, богатую историей"
    _ <- manyTill (skip (\x -> True)) dblCrnl
    return PostWelcome

dblCrnl :: A.Parser Word8
dblCrnl = do crnl
             crnl

unknownMessage :: A.Parser ServerEvent
unknownMessage = do
    endsIACGA
    return UnknownServerEvent

iacGA :: A.Parser Word8
iacGA = do iac
           ga

endsIACGA :: A.Parser Word8
endsIACGA = do skipWhile (/= iacWord)
               iac
               ga

iacWord :: Word8
iacWord = 255

iac :: A.Parser Word8
iac = A.word8 255

ga :: A.Parser Word8
ga = A.word8 249

cr :: A.Parser Word8
cr = A.word8 13

newline :: A.Parser Word8
newline = A.word8 10

crnl :: A.Parser Word8
crnl = do
    cr
    newline
