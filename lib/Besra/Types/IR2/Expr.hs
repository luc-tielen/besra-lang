
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.Expr
  ( Expr(..)
  , Decl(..)
  , Binding(..)
  , TypeAnn(..)
  ) where

import Protolude
import Besra.Types.Ann
import Besra.Types.Id
import Besra.Types.IR2.Lit
import Besra.Types.IR2.Pattern
import Besra.Types.IR2.Scheme


data Expr (ph :: Phase)
  = ELit (Ann ph) Lit
  | EVar (Ann ph) Id
  | ECon (Ann ph) Id
  | ELam (Ann ph) [Pattern] (Expr ph)
  | EApp (Ann ph) (Expr ph) (Expr ph)
  | EIf (Ann ph) (Expr ph) (Expr ph) (Expr ph)
  | ECase (Ann ph) (Expr ph) [(Pattern, Expr ph)]
  | ELet (Ann ph) [Decl ph] (Expr ph)

data Decl (ph :: Phase)
  = TypeAnnDecl (TypeAnn ph)
  | BindingDecl (Binding ph)

data Binding (ph :: Phase)
  = Binding (Ann ph) Id (Expr ph)

data TypeAnn (ph :: Phase)
  = TypeAnn (Ann ph) Id (Scheme ph)

deriving instance Eq (Ann ph) => Eq (Expr ph)
deriving instance Eq (Ann ph) => Eq (Decl ph)
deriving instance Eq (Ann ph) => Eq (TypeAnn ph)
deriving instance Eq (Ann ph) => Eq (Binding ph)
deriving instance Show (Ann ph) => Show (Expr ph)
deriving instance Show (Ann ph) => Show (Decl ph)
deriving instance Show (Ann ph) => Show (TypeAnn ph)
deriving instance Show (Ann ph) => Show (Binding ph)

