-- | Contains all the parts of the damage system
module Shared.Components.Damage( Damage(..), DamageType(..), DamageModifier(..) ) where

import Apecs
import qualified Data.Map as M (Map)
import Data.Map (unionWith, empty)

-- | type of damage of the following:
--
-- Slash, Blunt and Pierce - Brute
--
-- Radiation, Causitc, Heat, Cold, Shock - Burn
--
-- Poison - Toxin
--
-- Integral - Structural
--
-- Bloodloss, Asphyxiation - Airloss
--
-- Nerve, Stamina, Genetic, Bleeding - General
data DamageType =
    Slash | Blunt | Pierce -- Brute
  | Radiation | Caustic | Heat | Cold | Shock -- Burn
  | Poison -- Toxin
  | Integral -- Structural
  | Bloodloss | Asphyxiation -- Airloss
  | Nerve | Stamina | Genetic | Bleeding -- General
  deriving (Eq, Show, Ord)

-- | usually used for damage resistances, denotes a modification to damage
data DamageModifier =
    DamageModifierPrecentage -- ^ modify the damage by a precentage
      DamageType -- ^ type of damage to modify
      Double -- ^ precentage from 0 - 100
  | DamageModifierNum -- ^ modify the damage by a number
      DamageType -- ^ type of damage to modify
      Double -- ^ a number
  deriving (Eq, Show)

-- | list of all damage of an entity
newtype Damage = Damage { getDamage :: (M.Map DamageType Double) } deriving Show

instance Semigroup Damage where
  (<>) (Damage dmg1) (Damage dmg2) = Damage $ unionWith (+) dmg1 dmg2

instance Monoid Damage where
  mempty = Damage empty

instance Component Damage where type Storage Damage = Map Damage
