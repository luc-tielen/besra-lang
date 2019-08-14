
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.Impl ( Impl(..) ) where

import Protolude
import Besra.Types.Ann
import Besra.Types.IR2.Expr ( Binding )
import Besra.Types.IR2.Pred


data Impl (ph :: Phase)
  = Impl (Ann ph) [Pred ph] (Pred ph) [Binding ph]

deriving instance AnnHas Eq ph => Eq (Impl ph)
deriving instance AnnHas Show ph => Show (Impl ph)

