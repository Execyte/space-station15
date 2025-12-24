module Game.Server(Server(..)) where

import Game.Server.World
import Control.Concurrent.STM.TVar

import Data.Text(Text)
import Data.Map(Map)
import Apecs(Entity)

-- | The server datatype that contains information that the server's side of simulation needs to know.
data Server = Server
  { world :: TVar World
  }
