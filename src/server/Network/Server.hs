module Network.Server(ServerNetworkInfo(..)) where

import Game.Intent(Intent)

import Network.Snapshot
import Network.Server.ConnectionStatus

import GHC.Weak(Weak)

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.IntMap.Strict(IntMap)
import Data.HashMap.Strict(HashMap)
import Apecs(Entity)

-- | This is where all information about the server's network is stored.
data ServerNetworkInfo = ServerNetworkInfo
  { players :: TVar (Map Text Entity) -- ^ Logged in players and their respective entities. Name mapped to the entity that they control.
  , logins :: TVar (Map Int Text) -- ^ Actively logged in players, connId mapped to the logged in name.
  , conns :: TVar (IntMap (Weak ThreadId, Connection)) -- ^ The active connections.
  , actions :: TBQueue (Entity, Intent) -- ^ A queue that contains actions of the player.
  , snapshots :: TVar (HashMap Int ComponentSnapshot) -- ^ A hashmap that contains last snapshots of every networked entity.
  }
