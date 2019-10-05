
{-# LANGUAGE TypeApplications, ScopedTypeVariables, UndecidableInstances #-}

module Besra.Types.Tyvar ( Tyvar(..) ) where

import Protolude
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Kind
import Besra.Types.Span


data Tyvar (ph :: Phase)
  = Tyvar (AnnTy ph) Id


deriving instance Eq (AnnTy ph) => Eq (Tyvar ph)
deriving instance Show (AnnTy ph) => Show (Tyvar ph)

instance HasSpan (AnnTy ph) => HasSpan (Tyvar ph) where
  span (Tyvar ann _) = span ann

instance HasKind (AnnTy ph) => HasKind (Tyvar ph) where
  kind (Tyvar ann _) = kind ann

