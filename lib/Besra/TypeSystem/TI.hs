{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Besra.TypeSystem.TI
  ( TI
  , runTI
  , Instantiate(..)
  , newTVar
  , freshInst
  , unify
  , trim
  ) where

import Protolude hiding (Type, force)
import qualified Data.List as List
import Besra.TypeSystem.FreeTypeVars
import Besra.TypeSystem.Instantiate
import Besra.TypeSystem.NameGen
import Besra.TypeSystem.Subst
import Besra.TypeSystem.Unify
import Besra.TypeSystem.Error
import Besra.Types.Kind
import Besra.Types.Span
import Besra.Types.Ann
import Besra.Types.IR3 ( Scheme(..), Qual, Type(..), Tyvar, sameTyvar )


newtype TI a
  = TI (ExceptT Error (StateT Subst Gen) a)
  deriving ( Functor, Applicative, Monad
           , MonadState Subst, MonadError Error )

instance MonadGen TI where
  fresh sp = TI . fresh sp


-- | Runs the type inference monad.
runTI :: TI a -> Either Error a
runTI (TI m) = runGen $ evalStateT (runExceptT m) mempty

-- | Tries to unify 2 types and updates the current substitution set.
unify :: Type PreTC -> Type PreTC -> TI ()
unify t1 t2 = do
  s <- get
  u <- mgu (apply s t1) (apply s t2)
  extSubst u

-- | Extends the current set of substitutions with the
--   substitutions passed in to this function.
extSubst :: Subst -> TI ()
extSubst s' = do
  s <- get
  modify $ const (s' <> s)

-- | Trims the current set of substitutions down to only substitutions
--   for variables in the set `vs`.
trim :: [Tyvar PreTC] -> TI ()
trim vs = do
  (Subst s) <- get
  let s' = [(v, t) | (v, t) <- s, contains v vs]
      force = length (ftv (map snd s'))
  force `seq` modify $ const (Subst s')
  where contains v = isJust . List.find (sameTyvar v)

-- | Returns a fresh type variable with a specific kind.
newTVar :: Span -> Kind -> TI (Type PreTC)
newTVar sp k = do
  v <- fresh sp k
  pure $ TVar v

-- | Instantiates a type scheme with fresh type variables.
freshInst :: Scheme PreTC -> TI (Qual PreTC Type)
freshInst (ForAll sp ks qt) = do
  ts <- traverse (newTVar sp) ks
  pure (inst ts qt)

