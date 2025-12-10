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

data ServerNetworkInfo = ServerNetworkInfo
  { players :: TVar (Map Text Entity)
  , logins :: TVar (Map Int Text)
  , conns :: TVar (IntMap (Weak ThreadId, Connection))
  , actions :: TBQueue (Entity, Intent)
  , snapshots :: TVar (HashMap Int ComponentSnapshot)
  }
