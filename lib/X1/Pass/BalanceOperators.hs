
module X1.Pass.BalanceOperators ( BalanceError(..), pass ) where

import Protolude hiding ( pass )
import X1.Types.Expr1.Module


data BalanceError = BalanceOperator
  deriving (Eq, Show)

pass :: Monad m => Module -> ExceptT BalanceError m Module
pass = pure
