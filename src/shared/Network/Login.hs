module Network.Login where

import Data.Text(Text)
import Codec.Serialise(Serialise)
import GHC.Generics(Generic)

newtype LoginName = LoginName Text
  deriving (Show, Eq, Ord, Generic)

instance Serialise LoginName
