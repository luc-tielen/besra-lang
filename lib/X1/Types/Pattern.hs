

module X1.Types.Pattern ( Pattern(..) ) where

import Protolude
import X1.Types.Id
import X1.Types.Lit


data Pattern = PWildcard
             | PLit Lit
             | PVar Id
             | PCon Id [Pattern]
             | PAs Id Pattern
             deriving (Eq, Show)

