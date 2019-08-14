
module Besra.Types.Kind ( Kind(..), HasKind(..) ) where

import Protolude


-- | Data type representing a kind like in Haskell.
data Kind
  = Star
  | KArr Kind Kind
  deriving (Eq, Show)


class HasKind a where
  kind :: a -> Kind

instance HasKind Kind where
  kind = identity

instance HasKind b => HasKind (a, b) where
  kind = kind . snd

