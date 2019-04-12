
module X1.Parser.Types.Tycon ( Tycon(..) ) where

import Protolude
import X1.Types

newtype Tycon = Tycon Id
  deriving (Eq, Show)

