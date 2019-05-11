
module X1.Types.Expr1.Tycon ( Tycon(..) ) where

import Protolude
import X1.Types.Id


newtype Tycon = Tycon Id
  deriving (Eq, Show)

