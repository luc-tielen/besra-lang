
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Impl ( Impl(..) ) where

import Protolude
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Expr ( Binding )


data Impl = Impl [Pred] Pred [Binding]
  deriving (Eq, Show)

