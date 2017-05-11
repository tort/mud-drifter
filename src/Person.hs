{-# LANGUAGE OverloadedStrings #-}

module Person (
  personHandle
) where

import Data.Text

personHandle :: Text -> Text
personHandle ":connect" = ":connected"
personHandle text = text
