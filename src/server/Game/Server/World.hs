module Game.Server.World(World, System', initWorld, packSnapshot, packWorld) where

import Apecs
import Game.Components
import Network.Apecs.Snapshot
import Types

makeWorld "World" [''Player, ''Position, ''Dirty]

type System' a = System World a

packSnapshot :: Entity -> System' ComponentSnapshot
packSnapshot ent = do
  newPos <- get ent :: System' (Maybe Position)
  pure $ ComponentSnapshot{pos = newPos}

packWorld :: System' WorldSnapshot
packWorld = do
  entsWithPos <- collect \(Position _ _, ent@(Entity entId)) -> Just ent
  snapshots <- mapM (\ent -> packSnapshot ent >>= \snapshot -> pure $ EntitySnapshot (ServerEntityId $ unEntity ent) snapshot) entsWithPos
  pure $ WorldSnapshot snapshots
