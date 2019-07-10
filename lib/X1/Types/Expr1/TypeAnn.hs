
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.TypeAnn ( TypeAnn(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Scheme
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span


data TypeAnn (ph :: Phase)
  = TypeAnn (Ann ph) Id (Scheme ph)

deriving instance Eq (Ann ph) => Eq (TypeAnn ph)
deriving instance Show (Ann ph) => Show (TypeAnn ph)

instance HasSpan (Ann ph) => HasSpan (TypeAnn ph) where
  span (TypeAnn ann _ _) = span ann
