
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.Tyvar ( Tyvar(..) ) where

import Protolude
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span


data Tyvar (ph :: Phase)
  = Tyvar (Ann ph) Id

deriving instance Eq (Ann ph) => Eq (Tyvar ph)
deriving instance Show (Ann ph) => Show (Tyvar ph)

instance HasSpan (Ann ph) => HasSpan (Tyvar ph) where
  span (Tyvar ann _) = span ann
