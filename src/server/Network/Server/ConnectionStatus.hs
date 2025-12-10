module Network.Server.ConnectionStatus(ConnectionStatus(..), Connection(..)) where

import Data.Text(Text)

import Network.Message

import Control.Concurrent.STM.TBQueue

-- | The connection status between the server and a client.
data ConnectionStatus =
    Connecting -- ^ Waiting for the login information to be sent.
  | LoggedIn Text -- ^ Logged in and active.
  | Disconnecting -- ^ Waiting to be properly removed from the game.
  deriving Show

-- | This is the definition of a connection.
data Connection = Connection
  { writeQueue :: TBQueue (ServerMessage Message) -- ^ The write queue contains data that needs to be sent to the client. This is where you would write server to client messages.
  , connId :: Int
  , connStatus :: ConnectionStatus
  }
