{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$), (<|>))
import Pipes.Concurrent
import Pipes.Network.TCP
import Pipes.ByteString (ByteString, stdout, toHandle)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as TE
import Data.Attoparsec.ByteString
import qualified Pipes.Prelude as PPR
import qualified Pipes.Parse as PP
import Pipes.Prelude.Text (stdinLn)
import qualified Pipes.Attoparsec as PA
import Pipes
import Data.Monoid
import System.IO as SIO (Handle, withFile, IOMode(WriteMode))
import Person

main :: IO ()
main = SIO.withFile "log" WriteMode $ runWithLog

runWithLog :: Handle -> IO ()
runWithLog logFile = do 
    (outLog, inLog) <- spawn unbounded
    (outPerson, inPerson) <- spawn unbounded
    forkIO $ do runEffect $ fromInput inPerson >-> person
    runEffect $ stdinLn >-> PPR.takeWhile(/= ":quit") >-> toOutput outPerson
    --runEffect $ stdinLn >-> PPR.takeWhile(/= ":quit") >-> PPR.map (\x -> x <> "\n") >-> toOutput outPerson

person :: Consumer TE.Text IO ()
person = do
  text <- await
  liftIO $ putStrLn $ TE.unpack $ personHandle text
  person

oldMain :: IO ()
oldMain = SIO.withFile "log" WriteMode $ \logFile ->
            connect "bylins.su" "4000" $ \(socket, addr) ->
            do (outLog, inLog) <- spawn unbounded
               (outConsole, inConsole) <- spawn unbounded
               forkIO $ do runEffect $ fromSocket socket (2^15) >-> toOutput (outConsole <> outLog)
                           performGC
               forkIO $ do runEffect $ fromInput inConsole >-> stdout
                           performGC
               forkIO $ do runEffect $ fromInput inLog >-> toHandle logFile
                           performGC
               runEffect $ stdinLn >-> PPR.takeWhile(/= ":quit") >-> PPR.map (\x -> TE.append x "\n") >-> PPR.map encodeUtf8  >-> toSocket socket

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

--parser :: PP.Parser ByteString
--parser = do
    --codepage <- zoom codepagePromptParser drawAl
    --name <- namePromptParser 
    --pass <- passwordPromptParser
    --(return codepage) <|> (return name) <|> (return pass)
