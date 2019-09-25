{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Besra.TypeSystem.NameGen
  ( GenT(..)
  , Gen
  , MonadGen(..)
  , runGenT
  , runGen
  ) where

import Protolude
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Kind
import Besra.Types.Tyvar


type Counter = Int

newtype GenT m a = GenT (StateT Counter m a)
  deriving (Functor, Applicative, Monad)

type Gen = GenT Identity

class Monad m => MonadGen m where
  fresh :: Span -> Kind -> m (Tyvar KindInferred)

instance Monad m => MonadGen (GenT m) where
  fresh sp k =
    GenT $ do
      ctr <- get
      modify (+ 1)
      pure $ Tyvar (sp, k) (Id $ "t" <> show ctr)  -- TODO #t?

instance MonadGen m => MonadGen (StateT r m) where
  fresh sp = lift . fresh sp

-- TODO remove
instance MonadGen m => MonadGen (ExceptT e m) where
  fresh sp = lift . fresh sp

runGenT :: Monad m => GenT m a -> m a
runGenT (GenT m) = evalStateT m 0

runGen :: Gen a -> a
runGen = runIdentity . runGenT

