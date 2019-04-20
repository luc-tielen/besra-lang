
{-# LANGUAGE DeriveFunctor #-}

module X1.Types.Module ( Decl(..), Module(..) ) where

import Protolude
import X1.Types.Id
import X1.Parser.Types.Scheme


data Decl = TypeDecl Id Scheme
  deriving (Eq, Show)

newtype Module a = Module [a]
  deriving (Eq, Show, Functor)

