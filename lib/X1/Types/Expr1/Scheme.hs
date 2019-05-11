
module X1.Types.Expr1.Scheme ( Scheme(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Type


data Scheme = Scheme [Pred] Type
  deriving (Eq, Show)

