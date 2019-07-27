
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR1.Pred ( Pred(..) ) where

import Protolude hiding ( Type )
import Besra.Types.IR1.Type
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span


data Pred (ph :: Phase)
  = IsIn (Ann ph) Id [Type ph]

deriving instance AnnHas Eq ph => Eq (Pred ph)
deriving instance AnnHas Show ph => Show (Pred ph)

instance HasSpan (Ann ph) => HasSpan (Pred ph) where
  span (IsIn ann _ _) = span ann
