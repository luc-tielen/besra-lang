
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR1
  ( Module(..)
  , Decl(..)
  , ConDecl(..)
  , ADTHead(..)
  , ADTBody
  , ADT(..)
  , Expr(..)
  , ExprDecl(..)
  , Binding(..)
  , FixityInfo(..)
  , Trait(..)
  , Impl(..)
  , TypeAnn(..)
  , Scheme(..)
  , Pred(..)
  , Type(..)
  , Tycon(..)
  , Tyvar(..)
  , String(..)
  , Lit(..)
  , Number(..)
  , Pattern(..)
  ) where

import Protolude hiding ( Type, Fixity )
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Fixity
import Besra.Types.Tycon
import Besra.Types.Tyvar


newtype Module (ph :: Phase)
  = Module
  { unModule :: [Decl ph]
  }

data Decl (ph :: Phase)
  = TypeAnnDecl (TypeAnn ph)
  | DataDecl (ADT ph)
  | TraitDecl (Trait ph)
  | ImplDecl (Impl ph)
  | BindingDecl (Binding ph)
  | FixityDecl (FixityInfo ph)

data ADT (ph :: Phase)
  = ADT (Ann ph) (ADTHead ph) (ADTBody ph)

data ADTHead ph = ADTHead (Tycon ph) [Tyvar ph]

type ADTBody ph = [ConDecl ph]

data ConDecl (ph :: Phase)
  = ConDecl (Ann ph) Id [Type ph]

data TypeAnn (ph :: Phase)
  = TypeAnn (Ann ph) Id (Scheme ph)

data Impl (ph :: Phase)
  = Impl (Ann ph) [Pred ph] (Pred ph) [Binding ph]

data Trait ph
  = Trait (Ann ph) [Pred ph] (Pred ph) [TypeAnn ph]

data Scheme (ph :: Phase)
  = Scheme (Ann ph) [Pred ph] (Type ph)

data Pred (ph :: Phase)
  = IsIn (Ann ph) Id [Type ph]

data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) [Type ph]
  | TParen (Ann ph) (Type ph)

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
  | ELam (Ann ph) [Pattern ph] (Expr ph)
  | EApp (Ann ph) (Expr ph) [Expr ph]
  | EBinOp (Ann ph) (Expr ph) (Expr ph) (Expr ph)  -- operator, left side, right side
  | ENeg (Ann ph) (Expr ph)                          -- negation operator
  | EIf (Ann ph) (Expr ph) (Expr ph) (Expr ph)     -- condition, true clause, false clause
  | ECase (Ann ph) (Expr ph) [(Pattern ph, Expr ph)]   -- expression to match on, multiple branches
  | ELet (Ann ph) [ExprDecl ph] (Expr ph)            -- bindings end result
  | EParens (Ann ph) (Expr ph)

data Pattern (ph :: Phase)
  = PWildcard (Ann ph)
  | PLit (Ann ph) Lit
  | PVar (Ann ph) Id
  | PCon (Ann ph) Id [Pattern ph]
  | PAs (Ann ph) Id (Pattern ph)

data Lit
  = LChar Char
  | LString String
  | LNumber Number
  deriving (Eq, Show)

-- | The language only supports text based strings,
--   they are not automatically lists of characters.
newtype String = String Text
  deriving (Eq, Show)

data Number
  = SInt Int
  | SHex Text
  | SBin Text
  deriving (Eq, Show)

instance AnnHas HasSpan ph => HasSpan (ADT ph) where
  span (ADT ann _ _) = span ann

instance AnnHas HasSpan ph => HasSpan (ADTHead ph) where
  span (ADTHead name vars) = span $ span name :| map span vars

instance AnnHas HasSpan ph => HasSpan (ConDecl ph) where
  span (ConDecl ann _ _) = span ann

instance AnnHas HasSpan ph => HasSpan (TypeAnn ph) where
  span (TypeAnn ann _ _) = span ann

instance HasSpan (Ann ph) => HasSpan (Impl ph) where
  span (Impl ann _ _ _) = span ann

instance HasSpan (Ann ph) => HasSpan (Trait ph) where
  span (Trait ann _ _ _) = span ann

instance AnnHas HasSpan ph => HasSpan (Scheme ph) where
  span (Scheme ann _ _) = span ann

instance HasSpan (Ann ph) => HasSpan (Pred ph) where
  span (IsIn ann _ _) = span ann

instance AnnHas HasSpan ph => HasSpan (Type ph) where
  span = \case
    TCon tycon -> span tycon
    TVar tyvar -> span tyvar
    TApp t ts -> span $ t :| ts
    TParen ann _ -> span ann

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

instance HasSpan (Ann ph) => HasSpan (Pattern ph) where
  span = \case
    PWildcard ann -> span ann
    PLit ann _ -> span ann
    PVar ann _ -> span ann
    PCon ann _ _ -> span ann
    PAs ann _ _ -> span ann

instance HasSpan (Ann ph) => HasSpan (Binding ph) where
  span (Binding ann _ _) = span ann

deriving instance AnnHas Eq ph => Eq (Scheme ph)
deriving instance AnnHas Show ph => Show (Scheme ph)
deriving instance AnnHas Eq ph => Eq (TypeAnn ph)
deriving instance AnnHas Show ph => Show (TypeAnn ph)
deriving instance AnnHas Eq ph => Eq (Impl ph)
deriving instance AnnHas Show ph => Show (Impl ph)
deriving instance AnnHas Eq ph => Eq (Trait ph)
deriving instance AnnHas Show ph => Show (Trait ph)
deriving instance AnnHas Eq ph => Eq (Expr ph)
deriving instance AnnHas Eq ph => Eq (ExprDecl ph)
deriving instance AnnHas Eq ph => Eq (Pattern ph)
deriving instance AnnHas Eq ph => Eq (Binding ph)
deriving instance AnnHas Show ph => Show (Expr ph)
deriving instance AnnHas Show ph => Show (ExprDecl ph)
deriving instance AnnHas Show ph => Show (Pattern ph)
deriving instance AnnHas Show ph => Show (Binding ph)
deriving instance AnnHas Eq ph => Eq (FixityInfo ph)
deriving instance AnnHas Show ph => Show (FixityInfo ph)
deriving instance AnnHas Eq ph => Eq (Type ph)
deriving instance AnnHas Show ph => Show (Type ph)
deriving instance AnnHas Eq ph => Eq (Pred ph)
deriving instance AnnHas Show ph => Show (Pred ph)
deriving instance AnnHas Eq ph => Eq (Decl ph)
deriving instance AnnHas Show ph => Show (Decl ph)
deriving instance AnnHas Eq ph => Eq (Module ph)
deriving instance AnnHas Show ph => Show (Module ph)
deriving instance AnnHas Eq ph => Eq (ConDecl ph)
deriving instance AnnHas Eq ph => Eq (ADTHead ph)
deriving instance AnnHas Eq ph => Eq (ADT ph)
deriving instance AnnHas Show ph => Show (ConDecl ph)
deriving instance AnnHas Show ph => Show (ADTHead ph)
deriving instance AnnHas Show ph => Show (ADT ph)

