
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Trait ( Trait(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Pred
import X1.Types.Expr1.TypeAnn
import X1.Types.Ann


data Trait ph = Trait [Pred ph] (Pred ph) [TypeAnn ph]

deriving instance Eq (Ann ph) => Eq (Trait ph)
deriving instance Show (Ann ph) => Show (Trait ph)
