
-- TODO remove
{-# OPTIONS_GHC -Wno-deprecations #-}

module Besra.Pass.TypeSystem
  ( pass
  , Error(..)
  ) where

import Protolude hiding ( pass)
import Besra.Types.Ann
import Besra.TypeSystem.Error
import Besra.TypeSystem.Infer
import Besra.TypeSystem.TypeClass
import Besra.Types.CompilerState
import qualified Besra.Types.IR3 as IR3

-- TODO add new phase that forces exprs to be enriched with type information
type TypeChecked = KindInferred

pass :: Monad m
     => CompilerState KindInferred
     -> IR3.Module KindInferred
     -> ExceptT Error m (IR3.Module TypeChecked)
pass _ m =
  let traitEnv = initialEnv
      initialAssumps = []
      subst = tiProgram traitEnv initialAssumps m
   in traceShow subst $ pure m  -- TODO use subst to fill in types in expr

