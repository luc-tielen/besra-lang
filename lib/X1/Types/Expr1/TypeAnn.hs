
module X1.Types.Expr1.TypeAnn ( TypeAnn(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Scheme
import X1.Types.Id


data TypeAnn = TypeAnn Id Scheme
  deriving (Eq, Show)

