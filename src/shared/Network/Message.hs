module Network.Message(
  -- * Envelopes
  ClientMessage(..),
  ServerMessage(..),

  -- * Payload
  Message(..)) where

import Data.Text(Text)
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)
import Game.Intent(Intent)
import Network.Snapshot

type ServerEntity = Int

-- | These are the payloads that the server and client pass around.
data Message =
    Ping | Pong
  | TryLogin Text Text | LoginSuccess ServerEntity | LoginFail
  | Action Intent
  | ComponentSnapshotPacket ServerEntity ComponentSnapshot
  deriving Show
  deriving Generic

instance Serialise Message

data ClientMessage a = Call Int a | Cast a
  deriving Show
  deriving Generic

data ServerMessage a = Reply Int a | Event a
  deriving Show
  deriving Generic

instance Serialise a => Serialise (ClientMessage a)
instance Serialise a => Serialise (ServerMessage a)
