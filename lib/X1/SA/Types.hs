module X1.SA.Types ( ConflictingTypeDecl(..)
                   , ConflictingBindingDecl(..)
                   , SAError(..) ) where

import Protolude
import X1.Types.Module


data ConflictingTypeDecl = ConflictingTypeDecl FilePath [Decl]
  deriving (Eq, Show)

data ConflictingBindingDecl = ConflictingBindingDecl FilePath [Decl]
  deriving (Eq, Show)

data SAError = ConflictingTypeDeclErr ConflictingTypeDecl
             | ConflictingBindingDeclErr ConflictingBindingDecl
  deriving (Eq, Show)
