
{-# LANGUAGE DeriveFunctor #-}

module X1.Types.Module ( Decl(..), Module(..) ) where

import Protolude hiding ( Fixity(..) )
import X1.Types.Id
import X1.Types.Fixity
import X1.Types.Expr1
import X1.Types.Expr1.Scheme


data Decl = TypeDecl Id Scheme
          | BindingDecl Id Expr1
          | FixityDecl Fixity Int Id
  deriving (Eq, Show)

newtype Module a = Module [a]
  deriving (Eq, Show, Functor)
