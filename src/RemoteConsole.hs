{-# LANGUAGE OverloadedStrings #-}

module RemoteConsole (
  initRemoteConsole
  , runRemoteConsole
) where

import Person
import Pipes.Concurrent hiding (send)
import qualified Pipes.Prelude as PPR
import qualified Pipes.Prelude.Text as PPT
import Pipes
import qualified Pipes.Parse as PP
import qualified Data.Text as TE
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as DTIO
import Pipes.Network.TCP
import Control.Concurrent.Async
import qualified Data.ByteString as B
import qualified Pipes.ByteString as PB
import qualified Data.ByteString.Char8 as DBC8
import Pipes.Attoparsec
import Parser
import System.IO
import Debug.Trace

initRemoteConsole :: IO (Output B.ByteString, Output TE.Text -> IO ())
initRemoteConsole = do
    remoteConsoleBox <- spawn $ bounded 1024
    return $ (fst remoteConsoleBox, runRemoteConsole $ snd remoteConsoleBox)

runRemoteConsole :: Input B.ByteString -> Output TE.Text -> IO ()
runRemoteConsole input output = do
  async $ serve (Host "localhost") "4000" $ \(sock, addr) -> do 
                                                async $ runEffect $ fromInput input >-> toSocket sock
                                                runEffect $ parseRemoteInput sock (fromSocket sock (2^15)) >-> extractText >-> PPR.map decodeUtf8 >-> toOutput output
                                                return ()
  return ()

extractText :: Pipe RemoteConsoleEvent B.ByteString IO ()
extractText = do evt <- await
                 handle evt
                 extractText
              where handle (RemoteUserInput txt) = yield txt
                    handle _ = return ()

parseRemoteInput :: Socket -> Producer B.ByteString IO () -> Producer RemoteConsoleEvent IO ()
parseRemoteInput sock src = do 
    (result, partial) <- liftIO $ PP.runStateT (parse remoteInputParser) src
    continue result partial
    where continue (Just (Left err)) _ = liftIO $ send sock "\nerror when parsing remote console input"
          continue Nothing _ = liftIO $ send sock "\nparsed entire stream"
          continue result@(Just (Right bs)) partial = do yield bs
                                                         parseRemoteInput sock partial

traceBS :: Pipe B.ByteString B.ByteString IO ()
traceBS = do msg <- await
             liftIO $ traceIO $ "message from remote console: " ++ DBC8.unpack msg
             yield msg
             traceBS
