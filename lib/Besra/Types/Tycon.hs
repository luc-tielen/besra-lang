
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.Tycon ( Tycon(..) ) where

import Protolude
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span


data Tycon (ph :: Phase)
  = Tycon (AnnTy ph) Id

deriving instance Eq (AnnTy ph) => Eq (Tycon ph)
deriving instance Show (AnnTy ph) => Show (Tycon ph)

instance HasSpan (AnnTy ph) => HasSpan (Tycon ph) where
  span (Tycon ann _) = span ann
