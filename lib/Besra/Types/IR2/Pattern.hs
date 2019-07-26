
module Besra.Types.IR2.Pattern ( Pattern(..) ) where

import Protolude
import Besra.Types.IR2.Lit
import Besra.Types.Id


data Pattern = PWildcard
             | PLit Lit
             | PVar Id
             | PCon Id [Pattern]
             | PAs Id Pattern
             deriving (Eq, Show)

