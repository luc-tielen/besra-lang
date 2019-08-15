
module Besra.Types.Id ( Id(..) ) where

import Protolude

newtype Id = Id Text
  deriving (Eq, Show, Ord)
