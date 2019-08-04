
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.Type ( Type(..), Tycon(..), Tyvar(..) ) where

import Protolude hiding ( Type )
import Besra.Types.Ann
import Besra.Types.Kind
import Besra.Types.Tyvar
import Besra.Types.Tycon


data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) (Type ph)

deriving instance AnnHas Eq ph => Eq (Type ph)
deriving instance AnnHas Show ph => Show (Type ph)

instance HasKind (AnnTy ph) => HasKind (Type ph) where
  kind = \case
    TCon con -> kind con
    TVar var -> kind var
    TApp t _ -> case kind t of
      KArr _ k -> k
      _ -> panic "Unreachable code"

