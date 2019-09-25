module Besra.TypeSystem.Assump
  ( Assump(..)
  , find
  ) where

import Protolude hiding ( find ) -- TODO rename find in this file
import Control.Monad.Fail ( MonadFail(..) )  -- TODO remove
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.IR3 ( Scheme )
import Besra.TypeSystem.Subst

-- | Assumption about the type of a variable.
data Assump
  = Id :>: Scheme KindInferred
  deriving (Eq, Show)

instance Substitutable Assump where
  apply s (name :>: sc) =
    name :>: apply s sc

instance FreeTypeVars Assump where
  ftv (_ :>: sc) = ftv sc

-- TODO remove fail
find :: MonadFail m => Id -> [Assump] -> m (Scheme KindInferred)
find i [] = fail ("Unbound identifier: " ++ show i)
find i ((i' :>: sc):as) =
  if i == i'
    then pure sc
    else find i as
