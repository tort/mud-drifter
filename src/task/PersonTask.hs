{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module PersonTask (
              ) where

import Protolude
import Event
import Control.Wire hiding ((.))
import qualified Control.Wire as W
import qualified Event as E
import Control.Arrow

person :: Monad m => Wire m (W.Event E.Event) (W.Event E.Event)
person = undefined
