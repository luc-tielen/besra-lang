
module X1.Parser.Types.Scheme ( Scheme(..) ) where

import Protolude hiding ( Type )
import X1.Parser.Types.Pred
import X1.Parser.Types.Type


data Scheme = Scheme [Pred] Type
  deriving (Eq, Show)

