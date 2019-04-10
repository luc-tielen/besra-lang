
module X1.Parser.Types.Number ( Number(..) ) where

import Protolude


data Number = SInt Int
            | SHex Text
            | SBin Text
            deriving (Eq, Show)
