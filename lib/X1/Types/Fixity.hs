
module X1.Types.Fixity ( Fixity(..) ) where

import Protolude hiding ( Fixity(..) )


data Fixity = L | R | M
  deriving (Eq, Show)

