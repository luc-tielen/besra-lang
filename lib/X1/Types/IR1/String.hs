
module X1.Types.IR1.String ( String(..) ) where

import Protolude


-- | The language only supports text based strings,
--   they are not automatically lists of characters.
newtype String = String Text
  deriving (Eq, Show)
