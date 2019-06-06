
module X1.Pass.BalanceOperators ( BalanceError(..), pass ) where

import Protolude hiding ( pass )
import X1.Types.Module


data BalanceError = BalanceOperator
  deriving (Eq, Show)

pass :: Monad m => Module Decl -> ExceptT BalanceError m (Module Decl)
pass = pure
