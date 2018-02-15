{-# LANGUAGE OverloadedStrings #-}

module ServerInputParser ( serverInputParser
                         , remoteInputParser
                         , RemoteConsoleEvent(..)
                         ) where

import Control.Applicative
import Pipes.Attoparsec
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as C
import Data.Text
import qualified Data.Text as T
import Data.Attoparsec.ByteString
import Data.Text.Encoding
import qualified Data.ByteString as B
import Data.Word8
import qualified Data.ByteString.Char8 as DBC8
import Event

data RemoteConsoleEvent = TelnetControlSeq | RemoteUserInput B.ByteString

serverInputParser :: A.Parser ServerEvent
serverInputParser = codepagePrompt
                    <|> loginPrompt
                    <|> passwordPrompt
                    <|> welcomePrompt
                    <|> postWelcome
                    <|> location
                    <|> move
                    <|> listEquipment
                    <|> darkness
                    <|> unknownMessage

codepagePrompt :: A.Parser ServerEvent
codepagePrompt = do
    iacWill
    iacWill
    iacWill
    C.endOfLine
    string "Using keytable"
    _ <- manyTill (skip (const True)) (string "Select one : ")
    return CodepagePrompt

iacWill :: A.Parser Word8
iacWill = do iac
             A.word8 251
             A.anyWord8

iacWont :: A.Parser Word8
iacWont = do iac
             wont
             A.anyWord8

iacDo :: A.Parser Word8
iacDo = do iac
           A.word8 doWord
           A.anyWord8

iacDont :: A.Parser Word8
iacDont = do iac
             A.word8 dontWord
             A.anyWord8

iacAny :: A.Parser Word8
iacAny = do iac
            A.anyWord8

remoteInputParser :: A.Parser RemoteConsoleEvent
remoteInputParser = telnetControlSeq <|> eol <|> utf8String
    where eol = do C.endOfLine
                   return $ RemoteUserInput ""
          utf8String = do text <- A.takeWhile (\x -> x /= 255 && not (C.isEndOfLine x))
                          eolOrIac <- A.anyWord8
                          return $ RemoteUserInput text

telnetControlSeq :: A.Parser RemoteConsoleEvent
telnetControlSeq = do iacWill <|> iacWont <|> iacDo <|> iacDont <|> iacAny
                      return TelnetControlSeq

loginPrompt :: A.Parser ServerEvent
loginPrompt = do
    string "    "
    C.endOfLine
    string " --------"
    _ <- manyTill (skip (const True)) (string $ encodeUtf8 "Введите имя персонажа (или \"новый\" для создания нового): ")
    return LoginPrompt

passwordPrompt :: A.Parser ServerEvent
passwordPrompt = do
    string $ encodeUtf8 "Персонаж с таким именем уже существует. Введите пароль : "
    iacGA
    return PasswordPrompt

welcomePrompt :: A.Parser ServerEvent
welcomePrompt = do
    C.endOfLine
    C.endOfLine
    string $ encodeUtf8 "Последний раз вы заходили к нам в"
    skipTillIACGA
    return WelcomePrompt

postWelcome :: A.Parser ServerEvent
postWelcome = do
    C.endOfLine
    string $ encodeUtf8 "  Добро пожаловать на землю Киевскую, богатую историей"
    _ <- manyTill (skip (const True)) dblCrnl
    return PostWelcome

location :: A.Parser ServerEvent
location = do
    cs
    string "1;36m"
    locationName <- takeTill (== _bracketleft)
    A.word8 _bracketleft
    locationId <- C.decimal
    A.word8 _bracketright
    _ <- manyTill (skip (const True)) clearColors
    return $ LocationEvent $ Location locationId (decodeUtf8 locationName)

move :: A.Parser ServerEvent
move = do
    string $ encodeUtf8 "Вы поплелись "
    option "" $ string $ encodeUtf8 "на "
    direction <- takeTill (== _period)
    A.word8 _period
    C.endOfLine
    return $ MoveEvent (decodeUtf8 direction)

clearColors :: A.Parser ()
clearColors = do
    cs
    string "0;0m"
    return ()

cs :: A.Parser ()
cs = do C.char '\ESC'
        C.char '['
        return ()

dblCrnl :: A.Parser ()
dblCrnl = do C.endOfLine
             C.endOfLine

darkness :: A.Parser ServerEvent
darkness = do string $ encodeUtf8 "Слишком темно..."
              return DarknessEvent

listEquipment :: A.Parser ServerEvent
listEquipment = do string $ encodeUtf8 "На вас надето:"
                   list <- emptyEquipList <|> many1 equipmentItem
                   return $ ListEquipmentEvent list
                     where emptyEquipList = do C.endOfLine
                                               C.skipSpace
                                               string $ encodeUtf8 "Вы голы, аки сокол."
                                               return []
                           bodyPart = body <|> head <|> legs <|> waist <|> rightHand <|> leftHand
                           body = do string $ encodeUtf8 "<на теле>"
                                     return Body
                           head = do string $ encodeUtf8 "<на голове>"
                                     return Head
                           legs = do string $ encodeUtf8 "<на ногах>"
                                     return Legs
                           waist = do string $ encodeUtf8 "<на поясе>"
                                      return Waist
                           rightHand = do string $ encodeUtf8 "<в правой руке>"
                                          return RightHand
                           leftHand = do string $ encodeUtf8 "<в левой руке>"
                                         return LeftHand
                           itemState = stateParser "<великолепно>" Excellent
                                       <|> stateParser "<очень хорошо>" VeryGood
                                       <|> stateParser "<хорошо>" Good
                           stateParser stateTxt stateVal = do cs
                                                              C.take 5
                                                              string $ encodeUtf8 stateTxt
                                                              cs
                                                              string "0;37m"
                                                              return stateVal
                           itemName = manyTill C.anyChar (string $ encodeUtf8 "  ")
                           equipmentItem = do C.endOfLine
                                              bp <- bodyPart
                                              skipMany1 C.space
                                              name <- itemName
                                              state <- itemState
                                              A.skipWhile (\c -> not $ C.isEndOfLine c)
                                              return $ (EquippedItem bp (decodeUtf8 $ DBC8.pack name), state)

unknownMessage :: A.Parser ServerEvent
unknownMessage = do
  txt <- takeTillEndOfLineOrGA
  return $ UnknownServerEvent txt

iacGA :: A.Parser Word8
iacGA = do iac
           ga

takeTillEndOfLineOrGA :: A.Parser B.ByteString
takeTillEndOfLineOrGA = do skipMany $ A.word8 _cr
                           txt <- takeTill (\w -> C.isEndOfLine w || w == iacWord)
                           eitherP C.endOfLine iacGA
                           return txt

takeTillIACGA :: A.Parser B.ByteString
takeTillIACGA = do txt <- takeTill (== iacWord)
                   iacGA
                   return txt

skipTillIACGA :: A.Parser Word8
skipTillIACGA = do skipWhile (/= iacWord)
                   iacGA

iacWord :: Word8
iacWord = 255

wontWord :: Word8
wontWord = 252

doWord :: Word8
doWord = 253

dontWord :: Word8
dontWord = 254

wont :: A.Parser Word8
wont = A.word8 wontWord

iac :: A.Parser Word8
iac = A.word8 iacWord

ga :: A.Parser Word8
ga = A.word8 249
