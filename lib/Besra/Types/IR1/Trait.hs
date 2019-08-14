
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR1.Trait ( Trait(..) ) where

import Protolude hiding ( Type )
import Besra.Types.IR1.Pred
import Besra.Types.IR1.TypeAnn
import Besra.Types.Ann
import Besra.Types.Span


data Trait ph
  = Trait (Ann ph) [Pred ph] (Pred ph) [TypeAnn ph]

deriving instance AnnHas Eq ph => Eq (Trait ph)
deriving instance AnnHas Show ph => Show (Trait ph)

instance HasSpan (Ann ph) => HasSpan (Trait ph) where
  span (Trait ann _ _ _) = span ann
