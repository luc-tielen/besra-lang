
{-# LANGUAGE TypeApplications, ScopedTypeVariables, UndecidableInstances #-}

module Besra.Types.Tycon ( Tycon(..) ) where

import Protolude
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Kind
import Besra.Types.Span


data Tycon (ph :: Phase)
  = Tycon (AnnTy ph) Id

-- NOTE: custom Eq instance to avoid bugs in typechecker
-- (would otherwise take spans into account during typechecking)
instance (Eq (AnnTy ph), ToTag ph) => Eq (Tycon ph) where
  Tycon ann1 c1 == Tycon ann2 c2 =
    eqAnn (toTag @ph) ann1 ann2 && c1 == c2

eqAnn :: Eq (AnnTy ph) => PhaseTag ph -> AnnTy ph -> AnnTy ph -> Bool
eqAnn TagPreTC ann1 ann2 = kind ann1 == kind ann2
eqAnn _ ann1 ann2 = ann1 == ann2

deriving instance Show (AnnTy ph) => Show (Tycon ph)

instance HasSpan (AnnTy ph) => HasSpan (Tycon ph) where
  span (Tycon ann _) = span ann

instance HasKind (AnnTy ph) => HasKind (Tycon ph) where
  kind (Tycon ann _) = kind ann
