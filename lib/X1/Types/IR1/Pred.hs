
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.IR1.Pred ( Pred(..) ) where

import Protolude hiding ( Type )
import X1.Types.IR1.Type
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span


data Pred (ph :: Phase)
  = IsIn (Ann ph) Id [Type ph]

deriving instance Eq (Ann ph) => Eq (Pred ph)
deriving instance Show (Ann ph) => Show (Pred ph)

instance HasSpan (Ann ph) => HasSpan (Pred ph) where
  span (IsIn ann _ _) = span ann
