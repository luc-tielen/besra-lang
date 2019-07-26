
module Besra.Types.IR2.Number ( Number(..) ) where

import Protolude


newtype Number = Number Int
  deriving (Eq, Show)
