module X1.SA.Types ( ConflictingTypeAnnDecl(..)
                   , ConflictingBindingDecl(..)
                   , MissingTopLevelTypeAnnDecl(..)
                   , MissingTopLevelBindingDecl(..)
                   , SAError(..)
                   , SemanticError(..)
                   ) where

import Protolude
import X1.Types.Module


data ConflictingTypeAnnDecl = ConflictingTypeAnnDecl FilePath [Decl]
  deriving (Eq, Show)

data ConflictingBindingDecl = ConflictingBindingDecl FilePath [Decl]
  deriving (Eq, Show)

data MissingTopLevelTypeAnnDecl = MissingTopLevelTypeAnnDecl FilePath Decl
  deriving (Eq, Show)

data MissingTopLevelBindingDecl = MissingTopLevelBindingDecl FilePath Decl
  deriving (Eq, Show)

data SAError = ConflictingTypeAnnDeclErr ConflictingTypeAnnDecl
             | ConflictingBindingDeclErr ConflictingBindingDecl
             | MissingTopLevelTypeAnnDeclErr MissingTopLevelTypeAnnDecl
             | MissingTopLevelBindingDeclErr MissingTopLevelBindingDecl
  deriving (Eq, Show)

newtype SemanticError = SemanticError [SAError]
  deriving (Eq, Show)

