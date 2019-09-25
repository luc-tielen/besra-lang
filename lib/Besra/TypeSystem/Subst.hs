module Besra.TypeSystem.Subst
  ( Subst
  , FreeTypeVars(..)
  , Substitutable(..)
  , (@@)
  , (+->)
  , nullSubst
  , merge
  ) where

import Protolude hiding (Type)
import Control.Monad.Fail (MonadFail(..))
import Data.List (intersect, lookup, nub, union)
import Besra.Types.IR3 (Scheme(..), Qual(..), Pred(..), Type(..), Tyvar(..))
import Besra.Types.Ann

type KI = KindInferred

-- | Data type representing a set of substitutions that can be made.
type Subst = [(Tyvar KI, Type KI)]


class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable (Type KI) where
  apply s (TVar u)  = fromMaybe (TVar u) $ lookup u s
  apply s (TApp l r) = TApp (apply s l) (apply s r)
  apply _ t         = t

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)

instance Substitutable (t KI) => Substitutable (Qual KI t) where
  apply s (ps :=> t) = apply s ps :=> apply s t

instance Substitutable (Pred KI) where
  apply s (IsIn ann i ts) = IsIn ann i (apply s ts)

instance Substitutable (Scheme KI) where
  apply s (ForAll ann ks qt) = ForAll ann ks (apply s qt)


-- TODO move to separate file?
class FreeTypeVars a where
  ftv :: a -> [Tyvar KI]

instance FreeTypeVars (Type KI) where
  ftv (TVar u)  = [u]
  ftv (TApp l r) = ftv l `union` ftv r
  ftv _         = []

instance FreeTypeVars a => FreeTypeVars [a] where
  ftv = nub . concatMap ftv

instance FreeTypeVars (t KI) => FreeTypeVars (Qual KI t) where
  ftv (ps :=> t) = ftv ps `union` ftv t

instance FreeTypeVars (Pred KI) where
  ftv (IsIn _ _ ts) = ftv ts

instance FreeTypeVars (Scheme KI) where
  ftv (ForAll _ _ qt) = ftv qt



(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

infixr 4 @@

-- TODO remove fail
merge :: MonadFail m => Subst -> Subst -> m Subst
merge s1 s2 =
  if agree
    then pure (s1 ++ s2)
    else fail "merge fails"
  where
    sameSubst v = apply s1 (TVar v) == apply s2 (TVar v)
    agree = all sameSubst (map fst s1 `intersect` map fst s2)

nullSubst :: Subst
nullSubst = []

-- TODO remove
(+->) :: Tyvar KI -> Type KI -> Subst
u +-> t = [(u, t)]

