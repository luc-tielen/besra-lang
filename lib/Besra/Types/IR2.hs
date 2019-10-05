
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2
  ( Module(..)
  , ADT(..)
  , ADTHead(..)
  , ADTBody
  , ConDecl(..)
  , Expr(..)
  , Decl(..)
  , TypeAnn(..)
  , Binding(..)
  , Trait(..)
  , Impl(..)
  , Scheme(..)
  , Pred(..)
  , Type(..)
  , Tyvar(..)
  , Tycon(..)
  , Pattern(..)
  , module Besra.Types.Lit
  , adtName
  , adtRefersTo
  , traitName
  , traitRefersTo
  ) where

import Protolude hiding ( Type )
import Unsafe ( unsafeHead )
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Kind
import Besra.Types.Tycon
import Besra.Types.Tyvar
import Besra.Types.Lit


newtype Module (ph :: Phase)
  = Module [Decl ph]

data Decl (ph :: Phase)
  = TypeAnnDecl (TypeAnn ph)
  | BindingDecl (Binding ph)

data ADT (ph :: Phase)
  = ADT (Ann ph) (ADTHead ph) (ADTBody ph)

data ADTHead ph = ADTHead Id (Type ph)

type ADTBody ph = [ConDecl ph]

data ConDecl (ph :: Phase)
  = ConDecl (Ann ph) Id (Type ph)

data Impl (ph :: Phase)
  = Impl (Ann ph) [Pred ph] (Pred ph) [Binding ph]

data Trait (ph :: Phase)
  = Trait (Ann ph) [Pred ph] (Pred ph) [TypeAnn ph]

data Expr (ph :: Phase)
  = ELit (Ann ph) Lit
  | EVar (Ann ph) Id
  | ECon (Ann ph) Id
  | ELam (Ann ph) [Pattern ph] (Expr ph)
  | EApp (Ann ph) (Expr ph) (Expr ph)
  | EIf (Ann ph) (Expr ph) (Expr ph) (Expr ph)
  | ECase (Ann ph) (Expr ph) [(Pattern ph, Expr ph)]
  | ELet (Ann ph) [Decl ph] (Expr ph)

data Binding (ph :: Phase)
  = Binding (Ann ph) Id (Expr ph)

data TypeAnn (ph :: Phase)
  = TypeAnn (Ann ph) Id (Scheme ph)

data Pattern ph
  = PWildcard (Ann ph)
  | PLit (Ann ph) Lit
  | PVar (Ann ph) Id
  | PCon (Ann ph) Id [Pattern ph]
  | PAs (Ann ph) Id (Pattern ph)

data Scheme (ph :: Phase)
  = Scheme (Ann ph) [Pred ph] (Type ph)

data Pred (ph :: Phase)
  = IsIn (Ann ph) Id [Type ph]

data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) (Type ph)


instance HasSpan (Ann ph) => HasSpan (Pred ph) where
  span (IsIn ann _ _) = span ann

instance AnnHas HasSpan ph => HasSpan (Type ph) where
  span = \case
    TCon tycon -> span tycon
    TVar tyvar -> span tyvar
    TApp t1 t2 -> span t1 <> span t2

instance HasKind (AnnTy ph) => HasKind (Type ph) where
  kind = \case
    TCon con -> kind con
    TVar var -> kind var
    TApp t _ -> case kind t of
      KArr _ k -> k
      _ -> panic "Unreachable code"

deriving instance AnnHas Eq ph => Eq (Module ph)
deriving instance AnnHas Show ph => Show (Module ph)
deriving instance AnnHas Eq ph => Eq (Type ph)
deriving instance AnnHas Show ph => Show (Type ph)
deriving instance AnnHas Eq ph => Eq (Pred ph)
deriving instance AnnHas Show ph => Show (Pred ph)
deriving instance AnnHas Eq ph => Eq (ConDecl ph)
deriving instance AnnHas Eq ph => Eq (ADTHead ph)
deriving instance AnnHas Eq ph => Eq (ADT ph)
deriving instance AnnHas Show ph => Show (ConDecl ph)
deriving instance AnnHas Show ph => Show (ADTHead ph)
deriving instance AnnHas Show ph => Show (ADT ph)
deriving instance AnnHas Eq ph => Eq (Expr ph)
deriving instance AnnHas Eq ph => Eq (Pattern ph)
deriving instance AnnHas Eq ph => Eq (Decl ph)
deriving instance AnnHas Eq ph => Eq (TypeAnn ph)
deriving instance AnnHas Eq ph => Eq (Binding ph)
deriving instance AnnHas Show ph => Show (Expr ph)
deriving instance AnnHas Show ph => Show (Pattern ph)
deriving instance AnnHas Show ph => Show (Decl ph)
deriving instance AnnHas Show ph => Show (TypeAnn ph)
deriving instance AnnHas Show ph => Show (Binding ph)
deriving instance AnnHas Eq ph => Eq (Trait ph)
deriving instance AnnHas Show ph => Show (Trait ph)
deriving instance AnnHas Eq ph => Eq (Impl ph)
deriving instance AnnHas Show ph => Show (Impl ph)
deriving instance AnnHas Eq ph => Eq (Scheme ph)
deriving instance AnnHas Show ph => Show (Scheme ph)

adtName :: ADT ph -> Id
adtName (ADT _ (ADTHead name _) _) = name

adtRefersTo :: ADT ph -> [Id]
adtRefersTo adt@(ADT _ _ conDecls) =
  uniq $ filter (/= name) $ concatMap getRefs conDecls
  where
    name = adtName adt
    uniq = map unsafeHead . group . sort
    getRefs (ConDecl _ _ ty) = getRefsTy ty
    getRefsTy = \case
      TCon (Tycon _ con) -> [con]
      TVar _ -> mempty
      TApp t1 t2 -> getRefsTy t1 <> getRefsTy t2

traitName :: Trait ph -> Id
traitName (Trait _ _ (IsIn _ name _) _) = name

traitRefersTo :: Trait ph -> [Id]
traitRefersTo t@(Trait _ ps _ tys) =
  uniq $ filter (/= name) $ map getRefs ps <> concatMap getRefsTy tys
  where
    name = traitName t
    uniq = map unsafeHead . group . sort
    getRefs (IsIn _ id _) = id
    getRefsTy (TypeAnn _ _ (Scheme _ ps' _)) = map getRefs ps'

