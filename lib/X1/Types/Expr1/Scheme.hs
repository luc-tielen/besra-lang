
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Scheme ( Scheme(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Type
import X1.Types.Ann
import X1.Types.Span


data Scheme (ph :: Phase)
  = Scheme (Ann ph) [Pred ph] (Type ph)

deriving instance Eq (Ann ph) => Eq (Scheme ph)
deriving instance Show (Ann ph) => Show (Scheme ph)

instance HasSpan (Ann ph) => HasSpan (Scheme ph) where
  span (Scheme ann _ _) = span ann
