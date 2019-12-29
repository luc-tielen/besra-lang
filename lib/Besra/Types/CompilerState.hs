
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.CompilerState
  ( CompilerState2(..)
  , CompilerState3(..)
  ) where

import Protolude
import Besra.Types.Ann
import qualified Besra.Types.IR2 as IR2
import qualified Besra.Types.IR3 as IR3
import Besra.TypeSystem.KindSolver as K


data CompilerState2 ph
  = CompilerState2 [IR2.ADT ph] [IR2.Trait ph] [IR2.Impl ph] K.Env

data CompilerState3 ph
  = CompilerState3 [IR2.ADT ph] [IR3.Trait ph] [IR3.Impl ph] K.Env

deriving instance (Eq (IR2.Ann ph), Eq (AnnTy ph)) => Eq (CompilerState2 ph)
deriving instance ( Eq (IR2.Ann ph), Eq (IR3.Ann ph)
                  , Eq (IR3.AnnCon ph), Eq (AnnTy ph)) => Eq (CompilerState3 ph)
deriving instance (Show (IR2.Ann ph), Show (AnnTy ph)) => Show (CompilerState2 ph)
deriving instance ( Show (IR2.Ann ph), Show (IR3.Ann ph)
                  , Show (IR3.AnnCon ph), Show (AnnTy ph)) => Show (CompilerState3 ph)

