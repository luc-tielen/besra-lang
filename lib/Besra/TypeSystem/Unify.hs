module Besra.TypeSystem.Unify
  ( Unify(..)
  , Match(..)
  ) where

import Protolude hiding ( Type )
import Control.Monad.Fail ( MonadFail(..) )
import Besra.TypeSystem.Subst
import Besra.Types.IR3 ( Pred(..), Type(..), Tyvar(..) )
import Besra.Types.Kind
import Besra.Types.Ann


type Pred' = Pred KindInferred
type Type' = Type KindInferred
type Tyvar' = Tyvar KindInferred

class Unify t where
  -- TODO remove monadfail
  mgu :: MonadFail m => t -> t -> m Subst

instance Unify Type' where
  mgu (TApp l r) (TApp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    pure (s2 @@ s1)
  mgu (TVar u) t = varBind u t
  mgu t (TVar u) = varBind u t
  mgu (TCon tc1) (TCon tc2)
    | tc1 == tc2 = pure nullSubst
  mgu _ _ = fail "types do not unify"

instance (Unify t, Substitutable t) => Unify [t] where
  mgu (x:xs) (y:ys) = do
    s1 <- mgu x y
    s2 <- mgu (apply s1 xs) (apply s1 ys)
    pure (s2 @@ s1)
  mgu [] [] = pure nullSubst
  mgu _ _ = fail "lists do not unify"

instance Unify Pred' where
  mgu = liftPred mgu

-- TODO remove monadfail
liftPred :: MonadFail m => ([Type'] -> [Type'] -> m a) -> Pred' -> Pred' -> m a
liftPred m (IsIn _ i ts) (IsIn _ i' ts')
  | i == i' = m ts ts'
  | otherwise = fail "classes differ"


-- TODO remove monadfail
varBind :: MonadFail m => Tyvar' -> Type' -> m Subst
varBind u t
  | t == TVar u = pure nullSubst
  | u `elem` ftv t = fail "occurs check fails"
  | kind u /= kind t = fail "kinds do not match"
  | otherwise = pure (u +-> t)

class Match t where
  -- TODO remove monadfail
  match :: MonadFail m => t -> t -> m Subst

instance Match Type' where
  match (TApp l r) (TApp l' r') = do
    sl <- match l l'
    sr <- match r r'
    merge sl sr
  match (TVar u) t
    | kind u == kind t = pure (u +-> t)
  match (TCon tc1) (TCon tc2)
    | tc1 == tc2 = pure nullSubst
  match _ _ = fail "types do not match"

instance Match t => Match [t] where
  match ts ts' = do
    ss <- zipWithM match ts ts'
    foldM merge nullSubst ss

instance Match Pred' where
  match = liftPred match
