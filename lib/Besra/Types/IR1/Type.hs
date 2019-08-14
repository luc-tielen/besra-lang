
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR1.Type ( Type(..), Tycon(..), Tyvar(..) ) where

import Protolude hiding ( Type )
import Besra.Types.Tycon
import Besra.Types.Tyvar
import Besra.Types.Ann
import Besra.Types.Span


data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) [Type ph]
  | TParen (Ann ph) (Type ph)

deriving instance AnnHas Eq ph => Eq (Type ph)
deriving instance AnnHas Show ph => Show (Type ph)

instance AnnHas HasSpan ph => HasSpan (Type ph) where
  span = \case
    TCon tycon -> span tycon
    TVar tyvar -> span tyvar
    TApp t ts -> span $ t :| ts
    TParen ann _ -> span ann

