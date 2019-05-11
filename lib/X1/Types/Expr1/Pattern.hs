

module X1.Types.Expr1.Pattern ( Pattern(..) ) where

import Protolude
import X1.Types.Id
import X1.Types.Expr1.Lit


data Pattern = PWildcard
             | PLit Lit
             | PVar Id
             | PCon Id [Pattern]
             | PAs Id Pattern
             deriving (Eq, Show)

