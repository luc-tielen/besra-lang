
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Tyvar ( Tyvar(..) ) where

import Protolude
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span


data Tyvar (ph :: Phase)
  = Tyvar (Ann ph) Id

deriving instance Eq (Ann ph) => Eq (Tyvar ph)
deriving instance Show (Ann ph) => Show (Tyvar ph)

instance HasSpan (Ann ph) => HasSpan (Tyvar ph) where
  span (Tyvar ann _) = span ann
