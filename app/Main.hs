module Main where

import Protolude hiding (Location, Down, Up, Left, Right, Dual, to)
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import TextShow
import TextShow.Generic
import Control.Lens hiding ((&))
import Data.Aeson hiding (Result(..))
import Data.Aeson.Encode.Pretty
import Pipes

import Data.Maybe
import qualified Pipes.Prelude as PP
import qualified Pipes.Attoparsec as PA
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as C
import Text.Pretty.Simple
import Pipes.Lift

import ServerInputParser
import Console
import Event
import Mapper
import Person
import World

main :: IO ()
main = return ()
type RoomId = Int

genod = Person { personName = "генод"
               , personPassword = "каркасный"
               , residence = MudServer "bylins.su" 4000
               }

  {-
runDrifter :: IO ()
runDrifter = do currentDir <- getCurrentDirectory
                world <- liftIO $ loadWorld currentDir
                evtLog <- openFile "evt.log" WriteMode
                toConsoleBox <- spawn $ newest 100
                toRemoteConsoleBox <- spawn $ newest 100
                toServerInputLoggerBox <- spawn $ newest 100
                toEvtLoggerBox <- spawn $ newest 100
                toDrifterBox <- spawn $ newest 100
                let commonOutput = (fst toConsoleBox) `mappend` (fst toRemoteConsoleBox) `mappend` (fst toServerInputLoggerBox) `mappend` (fst toEvtLoggerBox)
                    readConsoleInput = runEffect $ runSafeP $ consoleInput `catchP` onUserInputException >-> toOutput (fst toDrifterBox)
                    printConsoleOutput = runEffect $ (printWorldStats world >> fromInput (snd toConsoleBox)) >-> consoleOutput
                    runDrifter = runEffect $ runSafeP $ fromInput (snd toDrifterBox) >-> drifter world >-> commandExecutor (fst toDrifterBox) >-> (toOutput commonOutput)
                    emitPulseEvery = atomically $ PC.send (fst toDrifterBox) PulseEvent >> return ()
                    onUserInputException (SomeException e) = yield UserInputIOException
                    {-onUserInputException (SomeException e) = liftIO $ do hFlush serverInputLog
                                                                hClose serverInputLog
                                                                hFlush evtLog
                                                                hClose evtLog-}

                async $ runServerInputLogger (snd toServerInputLoggerBox)
                async $ runEvtLogger (snd toEvtLoggerBox) evtLog
                async $ printConsoleOutput
                async $ runDrifter
                async $ runRemoteConsole (fst toDrifterBox, snd toRemoteConsoleBox)
                repeatedTimer emitPulseEvery (sDelay 1)
                readConsoleInput

-}
