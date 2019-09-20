
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.CompilerState
  ( CompilerState(..)
  ) where

import Protolude
import Besra.Types.Ann
import Besra.Types.IR2 ( ADT, Trait, Impl )
import Besra.TypeSystem.KindSolver as K


data CompilerState (ph :: Phase)
  = CompilerState [ADT ph] [Trait ph] [Impl ph] K.Env

deriving instance AnnHas Eq ph => Eq (CompilerState ph)
deriving instance AnnHas Show ph => Show (CompilerState ph)
