
module X1.Types.Expr1 ( Expr1(..), ExprDecl(..) ) where

import Protolude hiding ( Fixity )
import X1.Types.Id
import X1.Types.Fixity
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Scheme
import X1.Types.Expr1.Pattern


data ExprDecl = ExprTypeDecl Id Scheme
              | ExprBindingDecl Id Expr1
              | ExprFixityDecl Fixity Int Id
              deriving (Eq, Show)

data Expr1 = E1Lit Lit
           | E1Var Id
           | E1Con Id
           | E1Lam [Pattern] Expr1
           | E1App Expr1 [Expr1]
           | E1If Expr1 Expr1 Expr1           -- condition, true clause, false clause
           | E1Case Expr1 [(Pattern, Expr1)]  -- expression to match on, multiple branches
           | E1Let [ExprDecl] Expr1           -- bindings end result
           deriving (Eq, Show)

