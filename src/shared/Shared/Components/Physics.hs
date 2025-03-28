module Shared.Components.Physics(Position(..), Velocity(..), Collidable(..)) where

import Linear
import Apecs

-- so BASICALLY like, we have the positions and like
-- when a player moves, we interpolate the position and wait for it to finish
-- we can keep everything fresh and pixel-based basically
-- but at the same time collision code will still check for rounded values unless it's a projectile

-- interpolation defined at Shared.Components.Animation (TODO)

-- V3 because z layers.

newtype Position = Position (V3 Double) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V3 Double) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

data Collidable = Collidable deriving Show
instance Component Collidable where type Storage Collidable = Map Collidable
