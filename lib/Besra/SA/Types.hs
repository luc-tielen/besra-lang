
{-# LANGUAGE UndecidableInstances #-}

module Besra.SA.Types ( ConflictingTypeAnnDecl(..)
                   , ConflictingBindingDecl(..)
                   , MissingTopLevelTypeAnnDecl(..)
                   , MissingTopLevelBindingDecl(..)
                   , SAError(..)
                   , SemanticError(..)
                   ) where

import Protolude
import qualified Besra.Types.IR1 as IR1
import Besra.Types.Ann

type Decl' = IR1.Decl Parsed

data ConflictingTypeAnnDecl = ConflictingTypeAnnDecl FilePath [Decl']

data ConflictingBindingDecl = ConflictingBindingDecl FilePath [Decl']

data MissingTopLevelTypeAnnDecl = MissingTopLevelTypeAnnDecl FilePath Decl'

data MissingTopLevelBindingDecl = MissingTopLevelBindingDecl FilePath Decl'

data SAError = ConflictingTypeAnnDeclErr ConflictingTypeAnnDecl
             | ConflictingBindingDeclErr ConflictingBindingDecl
             | MissingTopLevelTypeAnnDeclErr MissingTopLevelTypeAnnDecl
             | MissingTopLevelBindingDeclErr MissingTopLevelBindingDecl

newtype SemanticError = SemanticError [SAError]

deriving instance Eq (IR1.Decl Parsed) => Eq ConflictingTypeAnnDecl
deriving instance Eq (IR1.Decl Parsed) => Eq ConflictingBindingDecl
deriving instance Eq (IR1.Decl Parsed) => Eq MissingTopLevelTypeAnnDecl
deriving instance Eq (IR1.Decl Parsed) => Eq MissingTopLevelBindingDecl
deriving instance Eq (IR1.Decl Parsed) => Eq SAError
deriving instance Eq (IR1.Decl Parsed) => Eq SemanticError

deriving instance Show (IR1.Decl Parsed) => Show ConflictingTypeAnnDecl
deriving instance Show (IR1.Decl Parsed) => Show ConflictingBindingDecl
deriving instance Show (IR1.Decl Parsed) => Show MissingTopLevelTypeAnnDecl
deriving instance Show (IR1.Decl Parsed) => Show MissingTopLevelBindingDecl
deriving instance Show (IR1.Decl Parsed) => Show SAError
deriving instance Show (IR1.Decl Parsed) => Show SemanticError

