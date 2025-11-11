module Intent(Intent(..)) where

import Apecs
import Direction(Direction)
import qualified Direction as Dir

data Intent =
    Quit
  | Wait
  | Move Direction
  deriving (Eq, Show)
