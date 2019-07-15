
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR1.Expr
  ( Expr(..)
  , ExprDecl(..)
  , Binding(..)
  , FixityInfo(..)
  ) where

import Protolude hiding ( Fixity )
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Fixity
import Besra.Types.IR1.Lit
import Besra.Types.IR1.Pattern
import Besra.Types.IR1.TypeAnn


data Binding (ph :: Phase)
  = Binding (Ann ph) Id (Expr ph)

data ExprDecl (ph :: Phase)
  = ExprTypeAnnDecl (TypeAnn ph)
  | ExprBindingDecl (Binding ph)
  | ExprFixityDecl (FixityInfo ph)

data FixityInfo (ph :: Phase)
  = FixityInfo (Ann ph) Fixity Int Id

data Expr (ph :: Phase)
  = ELit (Ann ph) Lit
  | EVar (Ann ph) Id
  | ECon (Ann ph) Id
  | ELam (Ann ph) [Pattern] (Expr ph)
  | EApp (Ann ph) (Expr ph) [Expr ph]
  | EBinOp (Ann ph) (Expr ph) (Expr ph) (Expr ph)  -- operator, left side, right side
  | ENeg (Ann ph) (Expr ph)                          -- negation operator
  | EIf (Ann ph) (Expr ph) (Expr ph) (Expr ph)     -- condition, true clause, false clause
  | ECase (Ann ph) (Expr ph) [(Pattern, Expr ph)]   -- expression to match on, multiple branches
  | ELet (Ann ph) [ExprDecl ph] (Expr ph)            -- bindings end result
  | EParens (Ann ph) (Expr ph)

deriving instance Eq (Ann ph) => Eq (FixityInfo ph)
deriving instance Show (Ann ph) => Show (FixityInfo ph)
deriving instance Eq (Ann ph) => Eq (ExprDecl ph)
deriving instance Show (Ann ph) => Show (ExprDecl ph)
deriving instance Eq (Ann ph) => Eq (Binding ph)
deriving instance Show (Ann ph) => Show( Binding ph)
deriving instance Eq (Ann ph) => Eq (Expr ph)
deriving instance Show (Ann ph) => Show (Expr ph)

instance HasSpan (Ann ph) => HasSpan (Expr ph) where
  span = \case
    ELit ann _ -> span ann
    EVar ann _ -> span ann
    ECon ann _ -> span ann
    ELam ann _ _ -> span ann
    EApp ann _ _ -> span ann
    EBinOp ann _ _ _ -> span ann
    ENeg ann _ -> span ann
    EIf ann _ _ _ -> span ann
    ECase ann _ _ -> span ann
    ELet ann _ _ -> span ann
    EParens ann _ -> span ann

instance HasSpan (Ann ph) => HasSpan (Binding ph) where
  span (Binding ann _ _) = span ann
