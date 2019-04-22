
module X1.Parser.Types.Tyvar ( Tyvar(..) ) where

import Protolude
import X1.Types.Id


newtype Tyvar = Tyvar Id
  deriving (Eq, Show)

