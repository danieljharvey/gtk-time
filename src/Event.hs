module Event where

import Data.Text (Text)

data Event = Greet Text | Salutation Text | Closed | TogglePlease
