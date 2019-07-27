
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.Trait ( Trait(..) ) where

import Protolude
import Besra.Types.Ann
import Besra.Types.IR2.Pred
import Besra.Types.IR2.Expr ( TypeAnn )


data Trait (ph :: Phase)
  = Trait (Ann ph) [Pred ph] (Pred ph) [TypeAnn ph]

deriving instance AnnHas Eq ph => Eq (Trait ph)
deriving instance AnnHas Show ph => Show (Trait ph)
