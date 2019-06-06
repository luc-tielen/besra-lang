
{-# LANGUAGE DeriveFunctor #-}

module X1.Types.Expr1.Module ( Decl(..), Module(..) ) where

import Protolude hiding ( Fixity(..) )
import X1.Types.Id
import X1.Types.Fixity
import X1.Types.Expr1.Expr
import X1.Types.Expr1.ADT
import X1.Types.Expr1.Trait
import X1.Types.Expr1.Impl
import X1.Types.Expr1.TypeAnn


data Decl = TypeAnnDecl TypeAnn
          | DataDecl ADT
          | TraitDecl Trait
          | ImplDecl Impl
          | BindingDecl Binding
          | FixityDecl Fixity Int Id
  deriving (Eq, Show)

newtype Module = Module [Decl]
  deriving (Eq, Show)

