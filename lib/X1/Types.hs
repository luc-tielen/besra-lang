
module X1.Types ( Id(..) ) where

import Protolude

newtype Id = Id Text
  deriving (Eq, Show)
