{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.Function                 ((&))
import           Data.Text                     (Text)
import           Pipes
import qualified Pipes.Extras                  as Pipes
import           Control.Monad                 (void)

import           GI.Gtk                        (Label (..), Window (..), Button (..), ListBox (..), ListBoxRow (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

import           GI.Gtk.Declarative.Container.Paned

data State =
  State { name :: Text
        , greeting :: Text
        , toggle :: Toggle
        }

iState :: State
iState = State { name = "", greeting = "", toggle = Maybe }

data Toggle = Yes | No | Maybe deriving (Show)

nextPlease :: Toggle -> Toggle
nextPlease Maybe = Yes
nextPlease Yes = No
nextPlease No = Maybe

showToggle :: Toggle -> Text
showToggle Maybe = "Maybe, perhaps"
showToggle Yes = "Totally, yes"
showToggle No = "Absolutely not"

data Event = Greet Text | Salutation Text | Closed | TogglePlease

view' :: State -> AppView Window Event
view' s =
  bin Window [#title := "Hello", on #deleteEvent (const (True, Closed)), #widthRequest := 400, #heightRequest := 300]
    $ paned
      [#wideHandle := True]
      (leftPane s)
      (rightPane s)

leftPane :: State -> Pane Event
leftPane s = pane PaneProperties { resize = True, shrink = False } child
  where child = widget Label [#label := (greeting s <> ",  " <> name s <> "!") ]

rightPane :: State -> Pane Event
rightPane s = pane PaneProperties { resize = True, shrink = False } child
  where child = container ListBox [] [ bin ListBoxRow [#activatable := False, #selectable := False] $
                                          widget Label [#label := (showToggle $ toggle s)
                                     ], bin ListBoxRow [#activatable := False, #selectable := False] $
                                          widget Button [ #label := "Next", on #clicked TogglePlease ]
                                     ]

update' :: State -> Event -> Transition State Event
update' state (Greet who) = Transition ( state { name = who }) (return Nothing)
update' state (Salutation which) = Transition ( state { greeting = which }) (return Nothing)
update' state TogglePlease = Transition ( state { toggle = (nextPlease $ toggle state) }) (return Nothing)
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
