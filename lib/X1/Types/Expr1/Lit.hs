
module X1.Types.Expr1.Lit ( Lit(..) ) where

import Protolude
import X1.Types.Expr1.String
import X1.Types.Expr1.Number


data Lit = LChar Char
         | LString String
         | LNumber Number
         deriving (Eq, Show)

