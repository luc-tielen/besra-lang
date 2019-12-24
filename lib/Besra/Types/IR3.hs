
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR3
  ( Module(..)
  , Trait(..)
  , Impl(..)
  , BindGroup
  , Explicit(..)
  , Implicit(..)
  , Expr(..)
  , Alt
  , Pred(..)
  , Type(..)
  , Tyvar(..)
  , Tycon(..)
  , Pattern(..)
  , Skolem(..)
  , SkolemScope(..)
  , module Besra.Types.Lit
  ) where

import Protolude hiding ( Type, Alt )
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Tycon
import Besra.Types.Tyvar
import Besra.Types.Lit


newtype Module (ph :: Phase)
  = Module [Explicit ph]

-- | A group of bindings, separated into explicitly and implicitly typed bindings.
type BindGroup ph = ([Explicit ph], [Implicit ph])

-- | An explicitly typed binding for a variable
data Explicit ph = Explicit Id (Type ph) [Alt ph]

-- | An implicitly typed binding for a variable
data Implicit ph = Implicit Id [Alt ph]

-- | Type representing a function binding
--   (left and right part of a function definition)
type Alt ph = ([Pattern ph], Expr ph)

data Trait (ph :: Phase)
  = Trait (Ann ph) [Pred ph] (Pred ph) (Map Id (Type ph))

data Impl (ph :: Phase)
  = Impl (Ann ph) [Pred ph] (Pred ph) [Explicit ph]

data Expr (ph :: Phase)
  = ELit (Ann ph) Lit
  | EVar (Ann ph) Id
  | ECon (Ann ph) Id (Type ph)
  | ELam (Ann ph) (Alt ph)
  | EApp (Ann ph) (Expr ph) (Expr ph)
  | EIf (Ann ph) (Expr ph) (Expr ph) (Expr ph)
  | ECase (Ann ph) (Expr ph) [(Pattern ph, Expr ph)]
  | ELet (Ann ph) (BindGroup ph) (Expr ph)

data Pattern ph
  = PWildcard (Ann ph)
  | PLit (Ann ph) Lit
  | PVar (Ann ph) Id
  | PCon (Ann ph) Id (Type ph) [Pattern ph]
  | PAs (Ann ph) Id (Pattern ph)

data Pred (ph :: Phase)
  = IsIn (Ann ph) Id [Type ph]

newtype SkolemScope = SkolemScope Int
  deriving (Eq, Ord, Show)

newtype Skolem = Skolem Int
  deriving (Eq, Show)

data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) (Type ph)
  | TUnknown Int                           -- Used only during unification, when having to guess a type
  | TSkolem (Tyvar ph) SkolemScope Skolem  -- Rigid type variable, only available in a specific scope
  | TForAll (Ann ph) Id (Maybe SkolemScope) (Type ph)

instance HasSpan (Ann ph) => HasSpan (Expr ph) where
  span = \case
    ELit ann _ -> span ann
    EVar ann _ -> span ann
    ECon ann _ _ -> span ann
    ELam ann _ -> span ann
    EApp ann _ _ -> span ann
    EIf ann _ _ _ -> span ann
    ECase ann _ _ -> span ann
    ELet ann _ _ -> span ann

instance HasSpan (Ann ph) => HasSpan (Pred ph) where
  span (IsIn ann _ _) = span ann

instance AnnHas HasSpan ph => HasSpan (Type ph) where
  span = \case
    TCon tycon -> span tycon
    TVar tyvar -> span tyvar
    TApp t1 t2 -> span t1 <> span t2
    TSkolem tyvar _ _ -> span tyvar
    TForAll ann _ _ _ -> span ann
    TUnknown _ -> panic "Attempt to call span on TUnknown"

deriving instance AnnHas Eq ph => Eq (Module ph)
deriving instance AnnHas Show ph => Show (Module ph)
deriving instance AnnHas Eq ph => Eq (Trait ph)
deriving instance AnnHas Show ph => Show (Trait ph)
deriving instance AnnHas Eq ph => Eq (Impl ph)
deriving instance AnnHas Show ph => Show (Impl ph)
deriving instance AnnHas Eq ph => Eq (Pred ph)
deriving instance AnnHas Show ph => Show (Pred ph)
deriving instance AnnHas Eq ph => Eq (Type ph)
deriving instance AnnHas Show ph => Show (Type ph)
deriving instance AnnHas Eq ph => Eq (Expr ph)
deriving instance AnnHas Show ph => Show (Expr ph)
deriving instance AnnHas Eq ph => Eq (Pattern ph)
deriving instance AnnHas Show ph => Show (Pattern ph)
deriving instance AnnHas Eq ph => Eq (Explicit ph)
deriving instance AnnHas Show ph => Show (Explicit ph)
deriving instance AnnHas Eq ph => Eq (Implicit ph)
deriving instance AnnHas Show ph => Show (Implicit ph)

