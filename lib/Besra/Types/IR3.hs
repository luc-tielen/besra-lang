
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR3
  ( Module(..)
  , BindGroup
  , Explicit(..)
  , Implicit(..)
  , Expr(..)
  , Impl(..)
  , Alt
  , Scheme(..)
  , Qual(..)
  , Pred(..)
  , Type(..)
  , Tyvar(..)
  , Tycon(..)
  , Pattern(..)
  , module Besra.Types.Lit
  ) where

import Protolude hiding ( Type, Alt )
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Kind
import Besra.Types.Tycon
import Besra.Types.Tyvar
import Besra.Types.Lit


newtype Module (ph :: Phase)
  = Module (BindGroup ph)

-- | A group of bindings, separated into explicitly and implicitly typed bindings.
--   The implicit bindings are grouped together in such a way that groups later
--   in the last can only depend on elements in current group or groups before it
--   (including also explicit bindings) for type inference to work as intended.
type BindGroup ph = ([Explicit ph], [[Implicit ph]])

-- | An explicitly typed binding for a variable
data Explicit ph = Explicit Id (Scheme ph) [Alt ph]

-- | An implicitly typed binding for a variable
data Implicit ph = Implicit Id [Alt ph]

-- | Type representing a function binding
--   (left and right part of a function definition)
type Alt ph = ([Pattern ph], Expr ph)

data Impl (ph :: Phase)
  = Impl (Ann ph) [Pred ph] (Pred ph) (BindGroup ph)

data Expr (ph :: Phase)
  = ELit (Ann ph) Lit
  | EVar (Ann ph) Id
  | ECon (Ann ph) Id (Scheme ph)
  | ELam (Ann ph) (Alt ph)
  | EApp (Ann ph) (Expr ph) (Expr ph)
  | EIf (Ann ph) (Expr ph) (Expr ph) (Expr ph)
  | ECase (Ann ph) (Expr ph) [(Pattern ph, Expr ph)]
  | ELet (Ann ph) (BindGroup ph) (Expr ph)

data Pattern ph
  = PWildcard
  | PLit Lit
  | PVar Id
  | PCon Id (Scheme ph) [Pattern ph]
  | PAs Id (Pattern ph)

data Qual ph a = [Pred ph] :=> a ph

data Scheme (ph :: Phase)
  = ForAll (Ann ph) [Kind] (Qual ph Type)

data Pred (ph :: Phase)
  = IsIn (Ann ph) Id [Type ph]

data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) (Type ph)
  | TGen Int

instance HasSpan (Ann ph) => HasSpan (Pred ph) where
  span (IsIn ann _ _) = span ann

instance AnnHas HasSpan ph => HasSpan (Type ph) where
  span = \case
    TCon tycon -> span tycon
    TVar tyvar -> span tyvar
    TApp t1 t2 -> span t1 <> span t2
    TGen _ -> panic "Attempt to call on span on TGen"

instance HasKind (AnnTy ph) => HasKind (Type ph) where
  kind = \case
    TCon con -> kind con
    TVar var -> kind var
    TApp t _ -> case kind t of
      KArr _ k -> k
      k -> panic $ "Unexpected kind on TApp: " <> show k
    TGen _ -> panic "Attempt to call kind on TGen"

deriving instance AnnHas Eq ph => Eq (Module ph)
deriving instance AnnHas Show ph => Show (Module ph)
deriving instance AnnHas Eq ph => Eq (Impl ph)
deriving instance AnnHas Show ph => Show (Impl ph)
deriving instance AnnHas Eq ph => Eq (Scheme ph)
deriving instance AnnHas Show ph => Show (Scheme ph)
deriving instance (AnnHas Eq ph, Eq (a ph)) => Eq (Qual ph a)
deriving instance (AnnHas Show ph, Show (a ph)) => Show (Qual ph a)
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

