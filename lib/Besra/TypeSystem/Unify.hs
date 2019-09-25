module Besra.TypeSystem.Unify
  ( Unify(..)
  , Match(..)
  ) where

import Protolude hiding ( Type )
import qualified Besra.TypeSystem.Subst as Subst
import Besra.TypeSystem.Subst ( Subst, Substitutable(..) )
import Besra.TypeSystem.FreeTypeVars
import Besra.TypeSystem.Error
import Besra.Types.IR3 ( Pred(..), Type(..), Tyvar(..) )
import Besra.Types.Kind
import Besra.Types.Ann


type Pred' = Pred KindInferred
type Type' = Type KindInferred
type Tyvar' = Tyvar KindInferred

class Unify t where
  mgu :: MonadError Error m => t -> t -> m Subst

instance Unify Type' where
  mgu (TApp l r) (TApp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    pure (s2 <> s1)
  mgu (TVar u) t = varBind u t
  mgu t (TVar u) = varBind u t
  mgu (TCon tc1) (TCon tc2)
    | tc1 == tc2 = pure mempty
  mgu t1 t2 = throwError $ UnificationFailure t1 t2

instance (Unify t, Substitutable t) => Unify [t] where
  mgu (x:xs) (y:ys) = do
    s1 <- mgu x y
    s2 <- mgu (apply s1 xs) (apply s1 ys)
    pure (s2 <> s1)
  mgu [] [] = pure mempty
  mgu ts1 ts2 =
    -- TODO improve error with ann? now hardly anything is known about the type
    throwError $ ListUnificationFailure (length ts1) (length ts2)

instance Unify Pred' where
  mgu = liftPred mgu

liftPred :: MonadError Error m
         => ([Type'] -> [Type'] -> m a)
         -> Pred' -> Pred' -> m a
liftPred m c1@(IsIn _ i ts) c2@(IsIn _ i' ts')
  | i == i' = m ts ts'
  | otherwise = throwError $ TraitMismatch c1 c2

varBind :: MonadError Error m => Tyvar' -> Type' -> m Subst
varBind u t
  | t == TVar u = pure mempty
  | u `elem` ftv t = throwError $ OccursCheck u t
  | kind u /= kind t = throwError $ KindMismatch u t (kind u) (kind t)
  | otherwise = pure $ Subst.singleton u t


class Match t where
  match :: MonadError Error m => t -> t -> m Subst

instance Match Type' where
  match (TApp l r) (TApp l' r') = do
    sl <- match l l'
    sr <- match r r'
    Subst.merge sl sr
  match (TVar u) t
    | kind u == kind t = pure $ Subst.singleton u t
  match (TCon tc1) (TCon tc2)
    | tc1 == tc2 = pure mempty
  match t1 t2 = throwError $ TypeMismatch t1 t2

instance Match t => Match [t] where
  match ts ts' = do
    ss <- zipWithM match ts ts'
    foldM Subst.merge mempty ss

instance Match Pred' where
  match = liftPred match

