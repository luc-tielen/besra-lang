
{-# LANGUAGE TypeApplications, ScopedTypeVariables, UndecidableInstances #-}

module Besra.Types.Tyvar ( Tyvar(..) ) where

import Protolude
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Kind
import Besra.Types.Span


data Tyvar (ph :: Phase)
  = Tyvar (AnnTy ph) Id

-- NOTE: custom Eq instance to avoid bugs in typechecker
-- (would otherwise take spans into account during typechecking)
instance (Eq (AnnTy ph), ToTag ph) => Eq (Tyvar ph) where
  Tyvar ann1 v1 == Tyvar ann2 v2 =
    eqAnn (toTag @ph) ann1 ann2 && v1 == v2

eqAnn :: Eq (AnnTy ph) => PhaseTag ph -> AnnTy ph -> AnnTy ph -> Bool
eqAnn TagPreTC ann1 ann2 = kind ann1 == kind ann2
eqAnn _ ann1 ann2 = ann1 == ann2

deriving instance Show (AnnTy ph) => Show (Tyvar ph)

instance HasSpan (AnnTy ph) => HasSpan (Tyvar ph) where
  span (Tyvar ann _) = span ann

instance HasKind (AnnTy ph) => HasKind (Tyvar ph) where
  kind (Tyvar ann _) = kind ann

