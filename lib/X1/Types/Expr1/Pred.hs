
module X1.Types.Expr1.Pred ( Pred(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Type
import X1.Types.Id


data Pred = IsIn Id [Type]
  deriving (Eq, Show)

