
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.Tycon ( Tycon(..) ) where

import Protolude
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span


data Tycon (ph :: Phase)
  = Tycon (Ann ph) Id

deriving instance Eq (Ann ph) => Eq (Tycon ph)
deriving instance Show (Ann ph) => Show (Tycon ph)

instance HasSpan (Ann ph) => HasSpan (Tycon ph) where
  span (Tycon ann _) = span ann
