
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.Pred ( Pred(..) ) where

import Protolude hiding ( Type )
import Besra.Types.Span
import Besra.Types.Ann
import Besra.Types.Id
import Besra.Types.IR2.Type


data Pred (ph :: Phase)
  = IsIn (Ann ph) Id [Type ph]

deriving instance AnnHas Eq ph => Eq (Pred ph)
deriving instance AnnHas Show ph => Show (Pred ph)

instance HasSpan (Ann ph) => HasSpan (Pred ph) where
  span (IsIn ann _ _) = span ann
