
module X1.Types.Lit ( Lit(..) ) where

import Protolude
import X1.Parser.Types.String
import X1.Parser.Types.Number


data Lit = LChar Char
         | LString String
         | LNumber Number
         deriving (Eq, Show)

