
module X1.Types.Expr1 ( Expr1(..), ExprDecl(..) ) where

import Protolude
import X1.Types.Id
import X1.Types.Lit
import X1.Parser.Types.Scheme


data ExprDecl = ExprTypeDecl Id Scheme
              | ExprBindingDecl Id Expr1
              deriving (Eq, Show)

data Expr1 = E1Lit Lit
           | E1Var Id
           | E1If Expr1 Expr1 Expr1  -- condition, true clause, false clause
           | E1Let [ExprDecl] Expr1     -- bindings end result
           deriving (Eq, Show)

