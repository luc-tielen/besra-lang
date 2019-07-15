
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR1.TypeAnn ( TypeAnn(..) ) where

import Protolude hiding ( Type )
import Besra.Types.IR1.Scheme
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span


data TypeAnn (ph :: Phase)
  = TypeAnn (Ann ph) Id (Scheme ph)

deriving instance Eq (Ann ph) => Eq (TypeAnn ph)
deriving instance Show (Ann ph) => Show (TypeAnn ph)

instance HasSpan (Ann ph) => HasSpan (TypeAnn ph) where
  span (TypeAnn ann _ _) = span ann
