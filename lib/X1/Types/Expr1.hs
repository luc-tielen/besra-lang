
module X1.Types.Expr1 ( Expr1(..) ) where

import Protolude
import X1.Types.Lit


newtype Expr1 = E1Lit Lit
  deriving (Eq, Show)

