module Game.Components(
  ServerEntity, ClientEntity,
  Camera(..), Me(..), NetEntity(..), Player(..), Position(..), Dirty(..)
) where

import GHC.Generics(Generic)

import Codec.Serialise(Serialise)

import Apecs
import Apecs.Experimental.Reactive

import Data.Semigroup
import Data.Monoid
import Data.Text(Text)

import Codec.Serialise(Serialise)

import GHC.Generics(Generic)

import Linear

type ServerEntity = Entity
type ClientEntity = Entity

-- Client to Server ID map
type ServerEntityId = Int

-- | TEMPORARY: remove this at some point and replace it for a newtype instead.
instance Serialise (V2 Float)

-- | Used to identify who you are as the client in game. Easier to query for this.
data Me = Me deriving Show
instance Component Me where
  type Storage Me = Unique Me

-- | The position of the camera, this should only be available to the client.
newtype Camera = Camera (V2 Float) deriving Show
instance Component Camera where
  type Storage Camera = Global Camera
instance Semigroup Camera where (Camera p1) <> (Camera p2) = Camera $ (p1 ^+^ p2)
instance Monoid Camera where mempty = Camera $ V2 0 0

-- | This is a mapping between server and client entity IDs.
newtype NetEntity = NetEntity ServerEntityId
  deriving newtype (Eq, Ord, Show, Enum)
instance Component NetEntity where
  type Storage NetEntity = Reactive (EnumMap NetEntity) (Map NetEntity)

-- | This is used on the server to identify entities owned by the player, usually stuff they control. It just contains their username.
newtype Player = Player Text
  deriving Show
  deriving Generic
instance Component Player where
  type Storage Player = Map Player
instance Serialise Player

-- | This is the position of any entity in the game world. Keep it as Int for servers, but floats for clients (for animation mostly.)
newtype Position = Position (V2 Float)
  deriving (Show, Eq)
  deriving Generic
instance Component Position where
  type Storage Position = Map Position
instance Serialise Position

-- | This is used to mark an entity that it's components have changed and need to be reconciled to the clients. You should, for the most part, always add this if you want the clients to see the changes.
data Dirty = Dirty deriving Show
instance Component Dirty where
  type Storage Dirty = Map Dirty
