module Game.Intent(Intent(..)) where

import Apecs
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)
import Game.Direction(Direction)

-- | Intent is the data type that contains information about what the player is doing. Send these to the server to be able to interact with the world.
data Intent =
    Quit
  | Wait
  | Move Direction
  deriving (Eq, Show)
  deriving Generic

instance Serialise Intent
