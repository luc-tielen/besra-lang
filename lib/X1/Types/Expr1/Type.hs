
module X1.Types.Expr1.Type ( Type(..), Tycon(..), Tyvar(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Tycon
import X1.Types.Expr1.Tyvar


data Type = TCon Tycon
          | TVar Tyvar
          | TApp Type [Type]
          deriving (Eq, Show)

