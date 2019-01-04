{-# LANGUAGE OverloadedStrings #-}

module State where

import Toggle (Toggle(..))
import Data.Text (Text)

data State =
  State { name :: Text
        , greeting :: Text
        , toggle :: Toggle
        }

iState :: State
iState = State { name = "", greeting = "", toggle = Maybe }
