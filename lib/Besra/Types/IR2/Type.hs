
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.Type ( Type(..), Tycon(..), Tyvar(..) ) where

import Protolude hiding ( Type )
import Besra.Types.Ann
import Besra.Types.Tyvar
import Besra.Types.Tycon


data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) (Type ph)

deriving instance Eq (Ann ph) => Eq (Type ph)
deriving instance Show (Ann ph) => Show (Type ph)
