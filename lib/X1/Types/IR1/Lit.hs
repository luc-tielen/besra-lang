
module X1.Types.IR1.Lit ( Lit(..) ) where

import Protolude
import X1.Types.IR1.String
import X1.Types.IR1.Number


data Lit = LChar Char
         | LString String
         | LNumber Number
         deriving (Eq, Show)

