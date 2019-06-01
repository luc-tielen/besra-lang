
module X1.Types.Expr1.Trait ( Trait(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Pred
import X1.Types.Expr1.TypeAnn


data Trait = Trait [Pred] Pred [TypeAnn]
  deriving (Eq, Show)

