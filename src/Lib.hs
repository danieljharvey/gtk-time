{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.Function                 ((&))
import           Data.Text                     (Text)
import           Pipes
import qualified Pipes.Extras                  as Pipes
import           Control.Monad                 (void)

import           GI.Gtk                        (Label (..), Window (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State =
  State { name :: Text
        , greeting :: Text
        }

iState :: State
iState = State { name = "", greeting = "" }

data Event = Greet Text | Salutation Text | Closed

view' :: State -> AppView Window Event
view' s =
  bin Window [#title := "Hello", on #deleteEvent (const (True, Closed)), #widthRequest := 400, #heightRequest := 300]
    $ widget Label [#label := (greeting s <> ",  " <> name s <> "!") ]

update' :: State -> Event -> Transition State Event
update' state (Greet who) = Transition ( state { name = who }) (return Nothing)
update' state (Salutation which) = Transition ( state { greeting = which }) (return Nothing)
update' _ Closed      = Exit

startApp :: IO ()
startApp = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = [greetings, salutations]
  , initialState = iState
  }
 where
  greetings =
    cycle ["Joe", "Mike"]
      & map (\n -> (Greet n))
      & Pipes.each
      & (>-> Pipes.delay 1.0)
  salutations =
    cycle ["Hello", "Goodbye"]
      & map (\n -> (Salutation n))
      & Pipes.each
      & (>-> Pipes.delay 1.5)
