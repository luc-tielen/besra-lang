
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Scheme ( Scheme(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Type
import X1.Types.Ann


data Scheme (ph :: Phase)
  = Scheme [Pred ph] (Type ph)

deriving instance Eq (Ann ph) => Eq (Scheme ph)
deriving instance Show (Ann ph) => Show (Scheme ph)
