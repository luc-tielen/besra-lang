
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.Pred ( Pred(..) ) where

import Protolude hiding ( Type )
import Besra.Types.Ann
import Besra.Types.Id
import Besra.Types.IR2.Type


data Pred (ph :: Phase)
  = IsIn (Ann ph) Id [Type ph]

deriving instance Eq (Ann ph) => Eq (Pred ph)
deriving instance Show (Ann ph) => Show (Pred ph)

