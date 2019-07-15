
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.IR1.Type ( Type(..), Tycon(..), Tyvar(..) ) where

import Protolude hiding ( Type )
import X1.Types.IR1.Tycon
import X1.Types.IR1.Tyvar
import X1.Types.Ann
import X1.Types.Span


data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) [Type ph]
  | TParen (Ann ph) (Type ph)

deriving instance Eq (Ann ph) => Eq (Type ph)
deriving instance Show (Ann ph) => Show (Type ph)

instance HasSpan (Ann ph) => HasSpan (Type ph) where
  span = \case
    TCon tycon -> span tycon
    TVar tyvar -> span tyvar
    TApp t ts -> span $ t :| ts
    TParen ann _ -> span ann
