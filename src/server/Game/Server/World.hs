module Game.Server.World(World, System', initWorld) where

import Apecs
import Game.Components

makeWorld "World" [''Player, ''Position, ''Dirty]

type System' a = System World a
