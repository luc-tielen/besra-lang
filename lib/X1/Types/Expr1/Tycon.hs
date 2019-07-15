
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Tycon ( Tycon(..) ) where

import Protolude
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span


data Tycon (ph :: Phase)
  = Tycon (Ann ph) Id

deriving instance Eq (Ann ph) => Eq (Tycon ph)
deriving instance Show (Ann ph) => Show (Tycon ph)

instance HasSpan (Ann ph) => HasSpan (Tycon ph) where
  span (Tycon ann _) = span ann
