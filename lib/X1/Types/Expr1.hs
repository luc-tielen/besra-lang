
module X1.Types.Expr1 ( Expr1(..) ) where

import Protolude
import X1.Types.Lit


data Expr1 = E1Lit Lit
           | E1If Expr1 Expr1 Expr1  -- condition, true clause, false clause
           deriving (Eq, Show)

