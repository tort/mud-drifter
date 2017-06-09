{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$), (<|>))
import Pipes.Concurrent as PC
import Pipes.Network.TCP as PNT
import Pipes.ByteString (ByteString, stdout, toHandle)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text as TE
import Data.Attoparsec.ByteString
import qualified Pipes.Prelude as PPR
import qualified Pipes.Parse as PP
import Pipes.Prelude.Text (stdinLn, stdoutLn)
import qualified Pipes.Attoparsec as PA
import Pipes
import Data.Monoid
import System.IO as SIO (hClose, openFile, Handle, withFile, IOMode(WriteMode))
import Person
import qualified Network.Socket.ByteString as NBS
import qualified Network.Socket as NS
import Data.Maybe
import Person
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Network.Simple.TCP as NST

main :: IO ()
main = runWithLog

runWithLog :: IO ()
runWithLog = do 
    (persToConsOut, persToConsIn) <- spawn unbounded
    (consToPersOut, consToPersIn) <- spawn unbounded
    keepConnectionEventSource <- newAddHandler
    disconnectEventSource <- newAddHandler
    network <- compile $ keepConnectedTask keepConnectionEventSource disconnectEventSource (connectToServer (snd disconnectEventSource) consToPersIn persToConsOut)
    actuate network
    forkIO $ do runEffect $ fromInput persToConsIn >-> stdout
                performGC
    runEffect $ stdinLn >-> PPR.takeWhile(/= ":quit") >-> sendToPerson keepConnectionEventSource consToPersOut

connectToServer :: Handler DisconnectEvent -> Input ByteString -> Output ByteString -> IO ()
connectToServer fireDisconnection consToPersIn persToConsOut = do 
    (sock, addr) <- NST.connectSock "bylins.su" "4000"
    logFile <- openFile "log" WriteMode
    (persToLogOut, persToLogIn) <- spawn unbounded
    let closeSockOnEof = NST.closeSock sock
    let closeLogFile = liftIO $ hClose logFile
    let fireDisconnectionEvent = liftIO $ fireDisconnection DisconnectEvent
    forkIO $ do runEffect $ (fromSocket sock (2^15) >> closeSockOnEof) >-> toOutput (persToConsOut <> persToLogOut) >> closeLogFile >> fireDisconnectionEvent
                performGC
    forkIO $ do runEffect $ fromInput persToLogIn >-> toHandle logFile
                performGC
    forkIO $ do runEffect $ fromInput consToPersIn >-> toSocket sock
                performGC
    return ()

sendToPerson :: EventSource KeepConnectionCommand -> Output ByteString -> Consumer TE.Text IO ()
sendToPerson keepConnectionEventSource consToPersOut = do
  text <- await
  toCommand text
  sendToPerson keepConnectionEventSource consToPersOut
  where toCommand ":conn" = liftIO $ fire keepConnectionEventSource $ KeepConnectionCommand True
        toCommand ":disconn" = liftIO $ fire keepConnectionEventSource $ KeepConnectionCommand False
        toCommand txt = liftIO $ do atomically $ PC.send consToPersOut (encodeUtf8 (TE.snoc txt '\n'))
                                    return ()

person :: Maybe Socket -> Output ByteString -> Consumer TE.Text IO ()
person socket persToConsOut = do
  text <- await
  personHandleInput socket persToConsOut text

personHandleInput :: Maybe PNT.Socket -> Output ByteString -> TE.Text -> Consumer TE.Text IO ()
personHandleInput (Just sock) persToConsOut text = do liftIO $ sendToServer sock $ text <> "\n"
                                                      person (Just sock) persToConsOut
personHandleInput mbsocket persToConsOut _ = do liftIO $ putStrLn "cannot execute"
                                                person mbsocket persToConsOut

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
