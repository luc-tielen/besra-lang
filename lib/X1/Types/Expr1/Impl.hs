
module X1.Types.Expr1.Impl ( Impl(..) ) where

import Protolude
import X1.Types.Expr1.Pred
import X1.Types.Expr1 ( Binding )


data Impl = Impl [Pred] Pred [Binding]
  deriving (Eq, Show)
