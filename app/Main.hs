{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$), (<|>))
import Pipes.Concurrent
import Pipes.Network.TCP as PNT
import Pipes.ByteString (ByteString, stdout, toHandle)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as TE
import Data.Attoparsec.ByteString
import qualified Pipes.Parse as PP
import Pipes.Prelude.Text (stdinLn, stdoutLn)
import qualified Pipes.Attoparsec as PA
import Pipes
import Data.Monoid
import System.IO as SIO (hClose, openFile, Handle, withFile, IOMode(WriteMode))
import qualified Network.Socket.ByteString as NBS
import qualified Network.Socket as NS
import Data.Maybe
import Person
import Console
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Network.Simple.TCP as NST
import qualified Data.ByteString.Char8 as DBC8

main :: IO ()
main = runWithLog

runWithLog :: IO ()
runWithLog = do 
    consoleCommandEventSource <- newAddHandler
    network <- compile $ keepConnectedTask consoleCommandEventSource DBC8.putStr
    actuate network
    runConsole (\command -> snd consoleCommandEventSource $ command)

codepagePromptParser :: Parser ByteString 
codepagePromptParser = do
    _ <- manyTill (skip (\x -> True)) (string "Select one :")
    return "codepage prompt"

namePromptParser :: Parser ByteString
namePromptParser = do
    _ <- manyTill (skip (\x -> True)) (string "Введите имя персонажа (или \"новый\" для создания нового):")
    return "person name"

passwordPromptParser :: Parser ByteString
passwordPromptParser = do
    _ <- manyTill (skip (\x -> True)) (string "Введите пароль :")
    return "password"
