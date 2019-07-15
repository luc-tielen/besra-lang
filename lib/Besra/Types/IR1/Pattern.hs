

module Besra.Types.IR1.Pattern ( Pattern(..) ) where

import Protolude
import Besra.Types.Id
import Besra.Types.IR1.Lit


data Pattern = PWildcard
             | PLit Lit
             | PVar Id
             | PCon Id [Pattern]
             | PAs Id Pattern
             deriving (Eq, Show)

