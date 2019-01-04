{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Concurrent.Async      (async)
import           Data.Function                 ((&))
import           Data.Text                     (Text)
import           Pipes
import qualified Pipes.Extras                  as Pipes
import           Control.Monad                 (void)

import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

import           GI.Gtk.Declarative.Container.Paned

import Styles (styles)

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

toggleClass :: Toggle -> [Text]
toggleClass Maybe = ["yello"]
toggleClass Yes   = ["green"]
toggleClass No    = ["red"]

data Event = Greet Text | Salutation Text | Closed | TogglePlease

view' :: State -> AppView Gtk.Window Event
view' s =
  bin Gtk.Window [#title := "Hello", on #deleteEvent (const (True, Closed)), #widthRequest := 400, #heightRequest := 300]
    $ paned
      [#wideHandle := True]
      (leftPane s)
      (rightPane s)

leftPane :: State -> Pane Event
leftPane s = pane PaneProperties { resize = True, shrink = True } child
  where child = widget Gtk.Label [#label := (greeting s <> ",  " <> name s <> "!") ]

rightPane :: State -> Pane Event
rightPane s = pane PaneProperties { resize = True, shrink = True } child
  where child = container Gtk.ListBox [] [ bin Gtk.ListBoxRow [#activatable := False, #selectable := False] $
                                          widget Gtk.Label [#label := (showToggle $ toggle s), classes (toggleClass $ toggle s)]
                                         , bin Gtk.ListBoxRow [#activatable := False, #selectable := False] $
                                          widget Gtk.Button [ #label := "Next", on #clicked TogglePlease ]
                                     ]

update' :: State -> Event -> Transition State Event
update' state (Greet who) = Transition ( state { name = who }) (return Nothing)
update' state (Salutation which) = Transition ( state { greeting = which }) (return Nothing)
update' state TogglePlease = Transition ( state { toggle = (nextPlease $ toggle state) }) (return Nothing)
update' _ Closed      = Exit

{-
startApp :: IO ()
startApp = void $ run app
-}

startApp :: IO ()
startApp = do
  void $ Gtk.init Nothing

  -- Set up screen and CSS provider
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  p      <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData p styles
  Gtk.styleContextAddProviderForScreen
    screen
    p
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  -- Start main loop
  void . async $ do
    void $ runLoop app
    Gtk.mainQuit
  Gtk.main

app :: App Gtk.Window State Event
app = App
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
