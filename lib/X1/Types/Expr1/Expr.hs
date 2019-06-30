
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Expr ( Expr1(..), ExprDecl(..), Binding(..) ) where

import Protolude hiding ( Fixity )
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span
import X1.Types.Fixity
import X1.Types.Expr1.Lit
import X1.Types.Expr1.Pattern
import X1.Types.Expr1.TypeAnn


data Binding (ph :: Phase)
  = Binding (Ann ph) Id (Expr1 ph)

data ExprDecl (ph :: Phase)
  = ExprTypeAnnDecl TypeAnn
  | ExprBindingDecl (Binding ph)
  | ExprFixityDecl (Ann ph) Fixity Int Id

data Expr1 (ph :: Phase)
  = E1Lit (Ann ph) Lit
  | E1Var (Ann ph) Id
  | E1Con (Ann ph) Id
  | E1Lam (Ann ph) [Pattern] (Expr1 ph)
  | E1App (Ann ph) (Expr1 ph) [Expr1 ph]
  | E1BinOp (Ann ph) (Expr1 ph) (Expr1 ph) (Expr1 ph)  -- operator, left side, right side
  | E1Neg (Ann ph) (Expr1 ph)                          -- negation operator
  | E1If (Ann ph) (Expr1 ph) (Expr1 ph) (Expr1 ph)     -- condition, true clause, false clause
  | E1Case (Expr1 ph) [(Pattern, Expr1 ph)]            -- expression to match on, multiple branches
  | E1Let [ExprDecl ph] (Expr1 ph)                     -- bindings end result
  | E1Parens (Ann ph) (Expr1 ph)

deriving instance Eq (Ann ph) => Eq (ExprDecl ph)
deriving instance Show (Ann ph) => Show (ExprDecl ph)
deriving instance Eq (Ann ph) => Eq (Binding ph)
deriving instance Show (Ann ph) => Show( Binding ph)
deriving instance Eq (Ann ph) => Eq (Expr1 ph)
deriving instance Show (Ann ph) => Show (Expr1 ph)

instance HasSpan (Ann ph) => HasSpan (Expr1 ph) where
  -- TODO handle cases without spans
  span = \case
    E1Lit ann _ -> span ann
    E1Var ann _ -> span ann
    E1Con ann _ -> span ann
    E1Lam ann _ _ -> span ann
    E1App ann _ _ -> span ann
    E1BinOp ann _ _ _ -> span ann
    E1Neg ann _ -> span ann
    E1If ann _ _ _ -> span ann
    E1Case _ _ -> panic "Not implemented!"
    E1Let _ _ -> panic "Not implemented!"
    E1Parens ann _ -> span ann

