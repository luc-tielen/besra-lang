module X1.SA.Types ( ConflictingTypeDecl(..)
                   , ConflictingBindingDecl(..)
                   , MissingTopLevelTypeDecl(..)
                   , MissingTopLevelBindingDecl(..)
                   , SAError(..) ) where

import Protolude
import X1.Types.Module


data ConflictingTypeDecl = ConflictingTypeDecl FilePath [Decl]
  deriving (Eq, Show)

data ConflictingBindingDecl = ConflictingBindingDecl FilePath [Decl]
  deriving (Eq, Show)

data MissingTopLevelTypeDecl = MissingTopLevelTypeDecl FilePath Decl
  deriving (Eq, Show)

data MissingTopLevelBindingDecl = MissingTopLevelBindingDecl FilePath Decl
  deriving (Eq, Show)

data SAError = ConflictingTypeDeclErr ConflictingTypeDecl
             | ConflictingBindingDeclErr ConflictingBindingDecl
             | MissingTopLevelTypeDeclErr MissingTopLevelTypeDecl
             | MissingTopLevelBindingDeclErr MissingTopLevelBindingDecl
  deriving (Eq, Show)

