
module X1.Parser.Types.Pred ( Pred(..) ) where

import Protolude hiding ( Type )
import X1.Parser.Types.Type
import X1.Types


data Pred = IsIn Id [Type]
  deriving (Eq, Show)

