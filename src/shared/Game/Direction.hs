module Game.Direction(Direction(..)) where

import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

-- | This is the direction datatype used for movement, changing orientation, comparing among other things.
data Direction = UP | DOWN | LEFT | RIGHT
  deriving (Eq, Show, Enum)
  deriving Generic

instance Serialise Direction
