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
import System.IO as SIO (Handle, withFile, IOMode(WriteMode))
import Person
import qualified Network.Socket.ByteString as NBS
import qualified Network.Simple.TCP as NST
import Data.Maybe

main :: IO ()
main = SIO.withFile "log" WriteMode $ runWithLog

runWithLog :: Handle -> IO ()
runWithLog logFile = do 
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
                                   forkIO $ do runEffect $ fromSocket sock (2^15) >-> toOutput persToConsOut
                                               performGC
                                   return sock

sendToServer :: Socket -> TE.Text -> IO ()
sendToServer socket text = do count <- NBS.send socket (encodeUtf8 text)
                              return ()

oldMain :: IO ()
oldMain = SIO.withFile "log" WriteMode $ \logFile ->
            PNT.connect "bylins.su" "4000" $ \(socket, addr) ->
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
