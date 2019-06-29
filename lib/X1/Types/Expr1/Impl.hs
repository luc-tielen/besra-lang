
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Impl ( Impl(..) ) where

import Protolude
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Expr ( Binding )
import X1.Types.Ann


data Impl (ph :: Phase)
  = Impl [Pred] Pred [Binding ph]

deriving instance Eq (Ann ph) => Eq (Impl ph)
deriving instance Show (Ann ph) => Show (Impl ph)
