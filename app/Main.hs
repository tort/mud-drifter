{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$), (<|>))
import Pipes.Concurrent
import Pipes.Network.TCP as PNT
import Pipes.ByteString (ByteString, stdout, toHandle)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as TE
import Data.Attoparsec.ByteString
import qualified Pipes.Prelude as PPR
import qualified Pipes.Parse as PP
import Pipes.Prelude.Text (stdinLn)
import qualified Pipes.Attoparsec as PA
import Pipes
import Data.Monoid
import System.IO as SIO (hClose, openFile, Handle, withFile, IOMode(WriteMode))
import Person
import qualified Network.Socket.ByteString as NBS
import qualified Network.Simple.TCP as NST
import qualified Network.Socket as NS
import Data.Maybe

main :: IO ()
main = runWithLog

runWithLog :: IO ()
runWithLog = do 
    (consToPersOut, consToPersIn) <- spawn unbounded
    (persToConsOut, persToConsIn) <- spawn unbounded
    forkIO $ do runEffect $ fromInput consToPersIn >-> person Nothing persToConsOut
                performGC
    forkIO $ do runEffect $ fromInput persToConsIn >-> stdout
                performGC
    runEffect $ stdinLn >-> PPR.takeWhile(/= ":quit") >-> toOutput consToPersOut

person :: Maybe Socket -> Output ByteString -> Consumer TE.Text IO ()
person socket persToConsOut = do
  text <- await
  personHandleInput socket persToConsOut text

personHandleInput :: Maybe PNT.Socket -> Output ByteString -> TE.Text -> Consumer TE.Text IO ()
personHandleInput Nothing persToConsOut ":conn" = do sock <- liftIO $ connectToServer persToConsOut
                                                     person (Just sock) persToConsOut
personHandleInput (Just sock) persToConsOut ":zap" = do NST.closeSock sock
                                                        person Nothing persToConsOut
personHandleInput (Just sock) persToConsOut text = do liftIO $ sendToServer sock $ text <> "\n"
                                                      person (Just sock) persToConsOut
personHandleInput mbsocket persToConsOut _ = do liftIO $ putStrLn "cannot execute"
                                                person mbsocket persToConsOut

connectToServer :: Output ByteString -> IO PNT.Socket
connectToServer persToConsOut = do (sock, addr) <- NST.connectSock "bylins.su" "4000"
                                   logFile <- openFile "log" WriteMode
                                   (persToLogOut, persToLogIn) <- spawn unbounded
                                   let closeSockOnEof = NST.closeSock sock
                                   let closeLogFile = liftIO $ hClose logFile
                                   forkIO $ do runEffect $ (fromSocket sock (2^15) >> closeSockOnEof >> closeLogFile) >-> toOutput (persToConsOut <> persToLogOut)
                                               performGC
                                   forkIO $ do runEffect $ fromInput persToLogIn >-> toHandle logFile
                                               performGC
                                   return sock

sendToServer :: Socket -> TE.Text -> IO ()
sendToServer socket text = do count <- NBS.send socket (encodeUtf8 text)
                              return ()

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
