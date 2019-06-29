
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Expr ( Expr1(..), ExprDecl(..), Binding(..) ) where

import Protolude hiding ( Fixity )
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Fixity
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Pattern
import X1.Types.Expr1.TypeAnn


data Binding (phase :: Phase)
  = Binding Id (Expr1 phase)

data ExprDecl (phase :: Phase)
  = ExprTypeAnnDecl TypeAnn
  | ExprBindingDecl (Binding phase)
  | ExprFixityDecl Fixity Int Id

data Expr1 (phase :: Phase)
  = E1Lit (Ann phase) Lit
  | E1Var (Ann phase) Id
  | E1Con (Ann phase) Id
  | E1Lam [Pattern] (Expr1 phase)
  | E1App (Expr1 phase) [(Expr1 phase)]
  | E1BinOp (Ann phase) (Expr1 phase) (Expr1 phase) (Expr1 phase)  -- operator, left side, right side
  | E1Neg (Ann phase) (Expr1 phase)                                -- negation operator
  | E1If (Expr1 phase) (Expr1 phase) (Expr1 phase)                 -- condition, true clause, false clause
  | E1Case (Expr1 phase) [(Pattern, (Expr1 phase))]                -- expression to match on, multiple branches
  | E1Let [ExprDecl phase] (Expr1 phase)                           -- bindings end result
  | E1Parens (Ann phase) (Expr1 phase)

deriving instance Eq (Ann phase) => Eq (ExprDecl phase)
deriving instance Show (Ann phase) => Show (ExprDecl phase)
deriving instance Eq (Ann phase) => Eq (Binding phase)
deriving instance Show (Ann phase) => Show( Binding phase)
deriving instance Eq (Ann phase) => Eq (Expr1 phase)
deriving instance Show (Ann phase) => Show (Expr1 phase)

