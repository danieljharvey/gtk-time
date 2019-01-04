{-# LANGUAGE OverloadedStrings #-}

module Toggle where

import           Data.Text                     (Text)

data Toggle = Yes | No | Maybe deriving (Show)

nextPlease :: Toggle -> Toggle
nextPlease Maybe = Yes
nextPlease Yes = No
nextPlease No = Maybe

showToggle :: Toggle -> Text
showToggle Maybe = "Maybe, perhaps"
showToggle Yes = "Totally, yes"
showToggle No = "Absolutely not"

toggleClass :: Toggle -> [Text]
toggleClass Maybe = ["yello"]
toggleClass Yes   = ["green"]
toggleClass No    = ["red"]
