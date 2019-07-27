
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR1.TypeAnn ( TypeAnn(..) ) where

import Protolude hiding ( Type )
import Besra.Types.IR1.Scheme
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span


data TypeAnn (ph :: Phase)
  = TypeAnn (Ann ph) Id (Scheme ph)

deriving instance AnnHas Eq ph => Eq (TypeAnn ph)
deriving instance AnnHas Show ph => Show (TypeAnn ph)

instance AnnHas HasSpan ph => HasSpan (TypeAnn ph) where
  span (TypeAnn ann _ _) = span ann
