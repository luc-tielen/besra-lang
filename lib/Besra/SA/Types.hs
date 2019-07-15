
{-# LANGUAGE UndecidableInstances #-}

module Besra.SA.Types ( ConflictingTypeAnnDecl(..)
                   , ConflictingBindingDecl(..)
                   , MissingTopLevelTypeAnnDecl(..)
                   , MissingTopLevelBindingDecl(..)
                   , SAError(..)
                   , SemanticError(..)
                   ) where

import Protolude
import Besra.Types.IR1.Module
import Besra.Types.Ann

type Decl' = Decl 'Parsed

data ConflictingTypeAnnDecl = ConflictingTypeAnnDecl FilePath [Decl']

data ConflictingBindingDecl = ConflictingBindingDecl FilePath [Decl']

data MissingTopLevelTypeAnnDecl = MissingTopLevelTypeAnnDecl FilePath Decl'

data MissingTopLevelBindingDecl = MissingTopLevelBindingDecl FilePath Decl'

data SAError = ConflictingTypeAnnDeclErr ConflictingTypeAnnDecl
             | ConflictingBindingDeclErr ConflictingBindingDecl
             | MissingTopLevelTypeAnnDeclErr MissingTopLevelTypeAnnDecl
             | MissingTopLevelBindingDeclErr MissingTopLevelBindingDecl

newtype SemanticError = SemanticError [SAError]

deriving instance Eq (Decl 'Parsed) => Eq ConflictingTypeAnnDecl
deriving instance Eq (Decl 'Parsed) => Eq ConflictingBindingDecl
deriving instance Eq (Decl 'Parsed) => Eq MissingTopLevelTypeAnnDecl
deriving instance Eq (Decl 'Parsed) => Eq MissingTopLevelBindingDecl
deriving instance Eq (Decl 'Parsed) => Eq SAError
deriving instance Eq (Decl 'Parsed) => Eq SemanticError

deriving instance Show (Decl 'Parsed) => Show ConflictingTypeAnnDecl
deriving instance Show (Decl 'Parsed) => Show ConflictingBindingDecl
deriving instance Show (Decl 'Parsed) => Show MissingTopLevelTypeAnnDecl
deriving instance Show (Decl 'Parsed) => Show MissingTopLevelBindingDecl
deriving instance Show (Decl 'Parsed) => Show SAError
deriving instance Show (Decl 'Parsed) => Show SemanticError

