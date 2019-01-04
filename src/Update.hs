module Update where

import State (State(..))
import Event (Event(..))
import           GI.Gtk.Declarative.App.Simple (Transition(..))

import Toggle (nextPlease)

update' :: State -> Event -> Transition State Event
update' state (Greet who) = Transition ( state { name = who }) (return Nothing)
update' state (Salutation which) = Transition ( state { greeting = which }) (return Nothing)
update' state TogglePlease = Transition ( state { toggle = (nextPlease $ toggle state) }) (return Nothing)
update' _ Closed      = Exit
