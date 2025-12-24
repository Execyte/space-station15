module Game.Server.Simulation (World, System', initWorld, step, act, networkSystem) where

import Apecs

import Game
import Game.Server.World

import Data.Maybe
import Data.HashMap.Strict(HashMap)
import Data.HashMap.Strict qualified as HashMap

import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Network.Message
import Network.Snapshot
import Network.Server
import Network.Server.ConnectionStatus
import Network.Server.NetStatus

-- | The act function is where the player's intents will be processed.
act :: Entity -> Intent -> System' ()
act ent (Move DOWN) = modify ent \(Position x y) -> (Position x (y - 1), Dirty)
act ent (Move UP) = modify ent \(Position x y) -> (Position x (y + 1), Dirty)
act ent (Move LEFT) = modify ent \(Position x y) -> (Position (x - 1) y, Dirty)
act ent (Move RIGHT) = modify ent \(Position x y) -> (Position (x + 1) y, Dirty)
act _ _ = pure ()

-- | Step the world once.
step :: Float -> System' ()
step dT = pure ()

-- | This is what handles sending component snapshots and whatnot to the player.
networkSystem :: NetStatus -> System' ()
networkSystem netstatus = do
  dirties <- collect \(Dirty, Entity ent) -> Just ent
  forM_ dirties \ent_ -> do
    snapshots <- lift $ readTVarIO netstatus.snapshots
    let ent = Entity ent_
    modify ent \Dirty -> Not @Dirty
    case HashMap.lookup ent_ snapshots of
      Just snapshot -> do
        newPos <- get ent :: System' (Maybe Position)
        let newSnapshot = ComponentSnapshot{pos = either' newPos (pos snapshot)}

        if newSnapshot /= snapshot then
          lift $ do
            conns <- readTVarIO netstatus.conns
            forM_ conns $ \(_, (Connection{writeQueue})) -> atomically $ writeTBQueue writeQueue (Event $ ComponentSnapshotPacket ent_ newSnapshot)
            atomically $ modifyTVar' netstatus.snapshots (HashMap.insert ent_ newSnapshot)
        else
          pure ()
      Nothing -> do
        newPos <- get ent :: System' (Maybe Position)
        let snapshot = ComponentSnapshot{pos = newPos}
        lift $ do
          conns <- readTVarIO netstatus.conns
          forM_ conns $ \(_, (Connection{writeQueue})) -> atomically $ writeTBQueue writeQueue (Event $ ComponentSnapshotPacket ent_ snapshot)
          atomically $ modifyTVar' netstatus.snapshots (HashMap.insert ent_ snapshot)
  where
    either' Nothing b = b
    either' a Nothing = a
    either' a b = if b == a then b else a
