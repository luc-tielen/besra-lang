
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.Scheme ( Scheme(..) ) where

import Protolude hiding ( Type )
import Besra.Types.Ann
import Besra.Types.IR2.Pred
import Besra.Types.IR2.Type


data Scheme (ph :: Phase)
  = Scheme (Ann ph) [Pred ph] (Type ph)

deriving instance AnnHas Eq ph => Eq (Scheme ph)
deriving instance AnnHas Show ph => Show (Scheme ph)
