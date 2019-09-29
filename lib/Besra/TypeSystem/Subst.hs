module Besra.TypeSystem.Subst
  ( Subst(..)
  , Substitutable(..)
  , merge
  , singleton
  ) where

import Protolude hiding (Type)
import qualified Data.List as List (intersect, lookup)
import Besra.Types.IR3 (Scheme(..), Qual(..), Pred(..), Type(..), Tyvar(..))
import Besra.Types.Ann
import Besra.TypeSystem.Error


-- | Data type representing a set of substitutions that can be made.
newtype Subst = Subst [(Tyvar PreTC, Type PreTC)]
  deriving (Eq, Show)

instance Semigroup Subst where
  subst@(Subst s1) <> (Subst s2) = Subst s
    where s = [(u, apply subst t) | (u, t) <- s2] ++ s1

instance Monoid Subst where
  mempty = Subst mempty

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable (Type PreTC) where
  apply s v@(TVar u) = fromMaybe v $ lookup u s
  apply s (TApp l r) = TApp (apply s l) (apply s r)
  apply _ t = t

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)

instance Substitutable (t PreTC) => Substitutable (Qual PreTC t) where
  apply s (ps :=> t) = apply s ps :=> apply s t

instance Substitutable (Pred PreTC) where
  apply s (IsIn ann i ts) = IsIn ann i (apply s ts)

instance Substitutable (Scheme PreTC) where
  apply s (ForAll ann ks qt) = ForAll ann ks (apply s qt)


merge :: MonadError Error m => Subst -> Subst -> m Subst
merge subst1@(Subst s1) subst2@(Subst s2) =
  if agree
    then pure (Subst $ s1 ++ s2)
    else throwError $ MergeFail s1 s2
  where
    sameSubst v = apply subst1 (TVar v) == apply subst2 (TVar v)
    agree = all sameSubst (map fst s1 `List.intersect` map fst s2)

lookup :: Tyvar PreTC -> Subst -> Maybe (Type PreTC)
lookup v (Subst s) = List.lookup v s

singleton :: Tyvar PreTC -> Type PreTC -> Subst
singleton v t = Subst [(v, t)]

