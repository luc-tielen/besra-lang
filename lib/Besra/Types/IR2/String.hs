
module Besra.Types.IR2.String ( String(..) ) where

import Protolude


newtype String = String Text
  deriving (Eq, Show)

