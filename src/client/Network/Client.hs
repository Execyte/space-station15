module Network.Client(processEvent) where

import Apecs

import Control.Monad
import Control.Monad.Extra
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TBQueue

import Network.Message
import Network.Apecs.Snapshot
import Network.Client.ConnectionStatus

import Game
import Game.Client
import Game.Client.World

processEntitySnapshot :: EntitySnapshot -> System' ()
processEntitySnapshot (EntitySnapshot id snapshot) = do
  ent <- getNetEntity id >>= \case
           Just ent -> pure ent
           Nothing -> newEntity (NetEntity id)
  case snapshot.pos of
    Just (MkPosition pos') -> set ent $ MkPosition pos'
    Nothing -> pure ()

-- | Here is where you process random data that the server sends to you.
processEvent :: Client -> MessageFromServer -> IO ()
processEvent client (EntitySnapshotPacket entSnapshot) =
  (atomically $ tryReadTMVar client.world) >>= \case
    Just world -> runWith world $ processEntitySnapshot entSnapshot
    Nothing -> pure ()
processEvent client (WorldSnapshotPacket (WorldSnapshot xs)) =
  (atomically $ tryReadTMVar client.world) >>= \case
    Just world -> runWith world $ sequence_ $ map processEntitySnapshot xs
    Nothing -> pure ()
processEvent _ n = do
  putStrLn $ "Seen " <> show n
  pure ()

