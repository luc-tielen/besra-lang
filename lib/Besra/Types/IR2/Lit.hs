
module Besra.Types.IR2.Lit ( Lit(..) ) where

import Protolude
import Besra.Types.IR2.String
import Besra.Types.IR2.Number


data Lit = LChar Char
         | LString String
         | LNumber Number
         deriving (Eq, Show)

