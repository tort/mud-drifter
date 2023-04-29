module CommandExecutor where

import           Data.Heap
import qualified Data.Heap as H
import           Data.Text
import           Event
import           Network.Simple.TCP
import           Pipes
import qualified Pipes.Concurrent as PC
import           Pipes.Concurrent hiding (send)
import           Pipes.Network.TCP
import qualified Pipes.Prelude as PP
import           Protolude hiding (yield)

data Command = Command Int Text | Lock Int | Release Int deriving (Eq, Show)

instance Ord Command where
  compare (Command l _) (Command r _) = compare l r
  compare (Lock l) (Command r _) = compare l r
  compare (Command l _) (Lock r) = compare l r
  compare (Lock l) (Lock r) = compare l r
  compare (Release l) (Lock r) = compare l r
  compare (Lock l) (Release r) = compare l r
  compare (Release l) (Command r _) = compare l r
  compare (Command l _) (Release r) = compare l r

commandExecutor :: MonadIO m => Pipe Event ByteString m ()
commandExecutor = forever $ await >>= \case (SendToServer text) -> liftIO (putStrLn text) >> yield (encodeUtf8 $ snoc text '\n')
                                            (ConsoleInput text) -> yield text
                                            (ServerEvent (ParseError err)) -> liftIO $ print err
                                            _ -> return ()

{-
commandExecutor = exec H.empty
  where
    exec :: MonadIO m => MaxHeap Command -> Pipe Event ByteString m ()
    exec heap =
      await >>= \case
        (SendToServer text) ->
          (yield . renderCommand $ text) >> (liftIO $ putStrLn text) >>
          exec heap
        event@(SendOnPulse prio text)  -> exec (insert (Command prio text) heap)
        event@(LockPulse prio)  -> exec (insert (Lock prio) heap)
        event@(ReleasePulse prio)  -> exec (insert (Release prio) heap)
        PulseEvent ->
          case view heap of
            Nothing -> exec heap
            (Just (item@(Lock prio), rest)) ->
              exec heap
            (Just (item@(Release prio), rest)) ->
              exec (H.dropWhile (== item) rest)
            (Just (item@(Command prio text), rest)) ->
              (liftIO $ putStrLn text) >> (yield . renderCommand $ text) >>
              exec (H.dropWhile (== item) rest)
-}

renderCommand :: Text -> ByteString
renderCommand = encodeUtf8 . flip snoc '\n'

sendCommand :: Socket -> Text -> IO ()
sendCommand sock txt = do
  send sock $ encodeUtf8 $ snoc txt '\n'
  return ()
