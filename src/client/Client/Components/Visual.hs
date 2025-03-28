module Client.Components.Visual ( Sprite(..)
  , Keyframe(..)
  , Animation(..)
  ) where

import Apecs
import qualified Data.Text as T
import qualified SDL

import Linear (V4 (..), V2 (..))

data Sprite = Sprite { path :: T.Text
  , position :: V2 Double -- this is just an offset from the Position component in x and y only.
  , orientation :: V2 Double
  , color :: V4 Double
  , layer :: Int
  }
  deriving (Show, Eq)

instance Component Sprite where type Storage Sprite = Map Sprite

data Keyframe = Keyframe { time_span :: Float
  , kf_position :: V2 Double
  , kf_orientation :: V2 Double
  , kf_color :: V4 Double
  }
  deriving (Show, Eq)

data Animation = Animation { keyframes :: [Keyframe] }

instance Component Animation where type Storage Animation = Map Animation
