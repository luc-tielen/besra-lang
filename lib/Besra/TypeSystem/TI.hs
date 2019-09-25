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
import Control.Monad.Fail  -- TODO remove
import Besra.TypeSystem.Instantiate
import Besra.TypeSystem.NameGen
import Besra.TypeSystem.Subst
import Besra.TypeSystem.Unify
import Besra.Types.Kind
import Besra.Types.Span
import Besra.Types.Ann
import Besra.Types.IR3 ( Scheme(..), Qual, Type(..), Tyvar )
import qualified Data.Text as T

type KI = KindInferred

data Err = Err Text  -- TODO remove

newtype TI a
  = TI (ExceptT Err (StateT Subst Gen) a)
  deriving (Functor, Applicative, Monad, MonadState Subst, MonadError Err)

instance MonadGen TI where
  fresh sp = TI . fresh sp

-- TODO remove
instance MonadFail TI where
  fail str = throwError $ Err $ T.pack str


-- | Runs the type inference monad.
runTI :: TI a -> Either Err a
runTI (TI m) = runGen $ evalStateT (runExceptT m) nullSubst

-- | Tries to unify 2 types and updates the current substitution set.
unify :: Type KI -> Type KI -> TI ()
unify t1 t2 = do
  s <- get
  u <- mgu (apply s t1) (apply s t2)
  extSubst u

-- | Extends the current set of substitutions with the
--   substitutions passed in to this function.
extSubst :: Subst -> TI ()
extSubst s' = do
  s <- get
  modify $ const (s' @@ s)

-- | Trims the current set of substitutions down to only substitutions
--   for variables in the set `vs`.
trim :: [Tyvar KI] -> TI ()
trim vs = do
  s <- get
  let s' = [(v, t) | (v, t) <- s, v `elem` vs]
      force = length (ftv (map snd s'))
  force `seq` modify $ const s'

-- | Returns a fresh type variable with a specific kind.
newTVar :: Span -> Kind -> TI (Type KI)
newTVar sp k = do
  v <- fresh sp k
  pure $ TVar v

-- | Instantiates a type scheme with fresh type variables.
freshInst :: Scheme KI -> TI (Qual KI Type)
freshInst (ForAll sp ks qt) = do
  ts <- traverse (newTVar sp) ks
  pure (inst ts qt)

