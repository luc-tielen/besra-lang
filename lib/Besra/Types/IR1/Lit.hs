
module Besra.Types.IR1.Lit ( Lit(..) ) where

import Protolude
import Besra.Types.IR1.String
import Besra.Types.IR1.Number


data Lit = LChar Char
         | LString String
         | LNumber Number
         deriving (Eq, Show)

