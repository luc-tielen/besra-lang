
module Besra.Types.IR1.Number ( Number(..) ) where

import Protolude


data Number = SInt Int
            | SHex Text
            | SBin Text
            deriving (Eq, Show)
