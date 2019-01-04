{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Styles where

import           Data.ByteString               (ByteString)

styles :: ByteString
styles = mconcat
  [ "button { border: 5px solid gray; font-weight: 800; }"
  , ".selected { background: white; border: 2px solid black; }"
  -- Specific color classes:
  , ".red { color: red; }"
  , ".green { color: green; }"
  , ".blue { color: blue; }"
  , ".yellow { color: goldenrod; }"
  ]
