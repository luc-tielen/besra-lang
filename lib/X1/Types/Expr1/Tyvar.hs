
module X1.Types.Expr1.Tyvar ( Tyvar(..) ) where

import Protolude
import X1.Types.Id


newtype Tyvar = Tyvar Id
  deriving (Eq, Show)

