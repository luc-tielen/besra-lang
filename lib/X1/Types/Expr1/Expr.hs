
module X1.Types.Expr1.Expr ( Expr1(..), ExprDecl(..), Binding(..) ) where

import Protolude hiding ( Fixity )
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Fixity
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Pattern
import X1.Types.Expr1.TypeAnn


data Binding = Binding Id Expr1
  deriving (Eq, Show)

data ExprDecl = ExprTypeAnnDecl TypeAnn
              | ExprBindingDecl Binding
              | ExprFixityDecl Fixity Int Id
              deriving (Eq, Show)

data Expr1 = E1Lit Lit
           | E1Var Id
           | E1Con Id
           | E1Lam [Pattern] Expr1
           | E1App Expr1 [Expr1]
           | E1BinOp Expr1 Expr1 Expr1        -- operator, left side, right side
           | E1Neg Expr1                      -- negation operator
           | E1If Expr1 Expr1 Expr1           -- condition, true clause, false clause
           | E1Case Expr1 [(Pattern, Expr1)]  -- expression to match on, multiple branches
           | E1Let [ExprDecl] Expr1           -- bindings end result
           | E1Parens Ann Expr1
           deriving (Eq, Show)

