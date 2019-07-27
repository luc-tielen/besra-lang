
module Besra.Types.Kind ( Kind(..), HasKind(..) ) where

import Protolude


-- | Data type representing a kind like in Haskell.
data Kind
  = Star
  | KArr Kind Kind
  deriving (Eq, Show)


class HasKind a where
  kind :: a -> Kind

