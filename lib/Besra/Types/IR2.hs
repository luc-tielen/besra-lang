
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2
  ( String(..)
  , Number(..)
  , Lit(..)
  , Pattern(..)
  , Tyvar(..)
  , Tycon(..)
  , Type(..)
  , Pred(..)
  , Scheme(..)
  , Expr(..)
  , Binding(..)
  , TypeAnn(..)
  , Decl(..)
  , Module(..)
  , Trait(..)
  , Impl(..)
  , ConDecl(..)
  , ADTHead(..)
  , ADTBody
  , ADT(..)
  ) where

import Protolude hiding ( Type )
import Besra.Types.Ann
import Besra.Types.Id

-- TODO move to separate files in IR2 folder

newtype String = String Text
  deriving (Eq, Show)

newtype Number = Number Int
  deriving (Eq, Show)

data Lit = LChar Char
         | LString String
         | LNumber Number
         deriving (Eq, Show)

data Pattern = PWildcard
             | PLit Lit
             | PVar Id
             | PCon Id [Pattern]
             | PAs Id Pattern
             deriving (Eq, Show)

-- TODO move Tyvar and Tycon to common part like Id
data Tyvar (ph :: Phase)
  = Tyvar (Ann ph) Id
data Tycon (ph :: Phase)
  = Tycon (Ann ph) Id

data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) (Type ph)

data Pred (ph :: Phase)
  = IsIn (Ann ph) Id [Type ph]

data Scheme (ph :: Phase)
  = Scheme (Ann ph) [Pred ph] (Type ph)

data Expr (ph :: Phase)
  = ELit (Ann ph) Lit
  | EVar (Ann ph) Id
  | ECon (Ann ph) Id
  | ELam (Ann ph) [Pattern] (Expr ph)
  | EApp (Ann ph) (Expr ph) (Expr ph)
  | EIf (Ann ph) (Expr ph) (Expr ph) (Expr ph)
  | ECase (Ann ph) (Expr ph) [(Pattern, Expr ph)]
  | ELet (Ann ph) [Decl ph] (Expr ph)

data Binding (ph :: Phase)
  = Binding (Ann ph) Id (Expr ph)

data TypeAnn (ph :: Phase)
  = TypeAnn (Ann ph) Id (Scheme ph)

data Decl (ph :: Phase)
  = TypeAnnDecl (TypeAnn ph)
  | BindingDecl (Binding ph)

newtype Module (ph :: Phase)
  = Module [Decl ph]


data Trait (ph :: Phase)
  = Trait (Ann ph) [Pred ph] (Pred ph) [TypeAnn ph]

data Impl (ph :: Phase)
  = Impl (Ann ph) [Pred ph] (Pred ph) [Binding ph]

data ConDecl (ph :: Phase)
  = ConDecl (Ann ph) Id (Type ph)

data ADTHead ph = ADTHead Id (Type ph)

type ADTBody ph = [ConDecl ph]

data ADT (ph :: Phase) = ADT (Ann ph) (ADTHead ph) (ADTBody ph)


deriving instance Eq (Ann ph) => Eq (Expr ph)
deriving instance Eq (Ann ph) => Eq (Tyvar ph)
deriving instance Eq (Ann ph) => Eq (Tycon ph)
deriving instance Eq (Ann ph) => Eq (Type ph)
deriving instance Eq (Ann ph) => Eq (Pred ph)
deriving instance Eq (Ann ph) => Eq (Scheme ph)
deriving instance Eq (Ann ph) => Eq (TypeAnn ph)
deriving instance Eq (Ann ph) => Eq (Binding ph)
deriving instance Eq (Ann ph) => Eq (Decl ph)
deriving instance Eq (Ann ph) => Eq (Module ph)
deriving instance Eq (Ann ph) => Eq (Trait ph)
deriving instance Eq (Ann ph) => Eq (Impl ph)
deriving instance Eq (Ann ph) => Eq (ConDecl ph)
deriving instance Eq (Ann ph) => Eq (ADTHead ph)
deriving instance Eq (Ann ph) => Eq (ADT ph)
deriving instance Show (Ann ph) => Show (Expr ph)
deriving instance Show (Ann ph) => Show (Tyvar ph)
deriving instance Show (Ann ph) => Show (Tycon ph)
deriving instance Show (Ann ph) => Show (Type ph)
deriving instance Show (Ann ph) => Show (Pred ph)
deriving instance Show (Ann ph) => Show (Scheme ph)
deriving instance Show (Ann ph) => Show (TypeAnn ph)
deriving instance Show (Ann ph) => Show (Binding ph)
deriving instance Show (Ann ph) => Show (Decl ph)
deriving instance Show (Ann ph) => Show (Module ph)
deriving instance Show (Ann ph) => Show (Trait ph)
deriving instance Show (Ann ph) => Show (Impl ph)
deriving instance Show (Ann ph) => Show (ConDecl ph)
deriving instance Show (Ann ph) => Show (ADTHead ph)
deriving instance Show (Ann ph) => Show (ADT ph)

