
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.IR1.Impl ( Impl(..) ) where

import Protolude
import X1.Types.IR1.Pred
import X1.Types.IR1.Expr ( Binding )
import X1.Types.Ann
import X1.Types.Span


data Impl (ph :: Phase)
  = Impl (Ann ph) [Pred ph] (Pred ph) [Binding ph]

deriving instance Eq (Ann ph) => Eq (Impl ph)
deriving instance Show (Ann ph) => Show (Impl ph)

instance HasSpan (Ann ph) => HasSpan (Impl ph) where
  span (Impl ann _ _ _) = span ann
