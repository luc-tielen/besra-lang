module Besra.TypeSystem.Assump
  ( Assump(..)
  , findScheme
  ) where

import Protolude
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.IR3 ( Scheme )
import Besra.TypeSystem.FreeTypeVars
import Besra.TypeSystem.Subst
import Besra.TypeSystem.Error


type KI = KindInferred

-- | Assumption about the type of a variable.
data Assump = Id :>: Scheme KI
  deriving (Eq, Show)

instance Substitutable Assump where
  apply s (name :>: sc) =
    name :>: apply s sc

instance FreeTypeVars Assump where
  ftv (_ :>: sc) = ftv sc


findScheme :: MonadError Error m => Span -> Id -> [Assump] -> m (Scheme KI)
findScheme sp i = \case
  [] -> throwError $ UnboundIdentifier sp i
  ((i' :>: sc):as) ->
    if i == i'
      then pure sc
      else findScheme sp i as
