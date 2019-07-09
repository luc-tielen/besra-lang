
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Type ( Type(..), Tycon(..), Tyvar(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Tycon
import X1.Types.Expr1.Tyvar
import X1.Types.Ann


data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) [Type ph]

deriving instance Eq (Ann ph) => Eq (Type ph)
deriving instance Show (Ann ph) => Show (Type ph)
