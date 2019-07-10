
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Trait ( Trait(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Pred
import X1.Types.Expr1.TypeAnn
import X1.Types.Ann
import X1.Types.Span


data Trait ph
  = Trait (Ann ph) [Pred ph] (Pred ph) [TypeAnn ph]

deriving instance Eq (Ann ph) => Eq (Trait ph)
deriving instance Show (Ann ph) => Show (Trait ph)

instance HasSpan (Ann ph) => HasSpan (Trait ph) where
  span (Trait ann _ _ _) = span ann
