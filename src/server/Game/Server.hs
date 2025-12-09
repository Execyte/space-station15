module Game.Server(Server(..)) where

import Game.Server.Simulation
import Control.Concurrent.STM.TVar

import Data.Text(Text)
import Data.Map(Map)
import Apecs(Entity)

data Server = Server
  { world :: TVar World
  }
