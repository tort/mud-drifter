{-# LANGUAGE OverloadedStrings #-}

module Person (
  personHandle
  , personBot
) where

import Data.Text
import Reactive.Banana
import Reactive.Banana.Frameworks

data GoCommand = GoCommand

personHandle :: Text -> Text
personHandle ":connect" = ":connected"
personHandle text = text

personBot :: IO ()
personBot = do 
    (addGoCommandHandler, fireGoCommand) <- newAddHandler
    network <- compile $ personNetworkDesc addGoCommandHandler
    actuate network
    fireGoCommand GoCommand

personNetworkDesc :: AddHandler GoCommand -> MomentIO ()
personNetworkDesc addGoCommandHandler = do
    goCommandEvent <- fromAddHandler addGoCommandHandler
    reactimate $ putStrLn "go event fired" <$ goCommandEvent
