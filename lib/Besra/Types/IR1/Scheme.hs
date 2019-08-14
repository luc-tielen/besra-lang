
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR1.Scheme ( Scheme(..) ) where

import Protolude hiding ( Type )
import Besra.Types.IR1.Pred
import Besra.Types.IR1.Type
import Besra.Types.Ann
import Besra.Types.Span


data Scheme (ph :: Phase)
  = Scheme (Ann ph) [Pred ph] (Type ph)

deriving instance AnnHas Eq ph => Eq (Scheme ph)
deriving instance AnnHas Show ph => Show (Scheme ph)

instance AnnHas HasSpan ph => HasSpan (Scheme ph) where
  span (Scheme ann _ _) = span ann
