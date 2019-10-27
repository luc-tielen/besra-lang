
{-# LANGUAGE UndecidableInstances #-}

module Besra.SA.Types
  ( ConflictingTypeAnnDecl(..)
  , ConflictingBindingDecls(..)
  , ConflictingConstantDecl(..)
  , ConflictingArgCounts(..)
  , MissingTopLevelTypeAnnDecl(..)
  , MissingTopLevelBindingDecl(..)
  , SAError(..)
  , SemanticError(..)
  ) where

import Protolude
import Besra.Types.IR1.Module
import Besra.Types.IR1.Expr
import Besra.Types.Ann

type Decl' = Decl Parsed
type Binding' = Binding Parsed

data ConflictingTypeAnnDecl
  = ConflictingTypeAnnDecl FilePath [Decl']

data ConflictingConstantDecl
  = ConflictingConstantDecl FilePath [Binding']

data ConflictingBindingDecls
  = ConflictingBindingDecls FilePath [Binding'] [Binding']

data ConflictingArgCounts
  = ConflictingArgCounts FilePath [Binding']

data MissingTopLevelTypeAnnDecl
  = MissingTopLevelTypeAnnDecl FilePath Decl'

data MissingTopLevelBindingDecl
  = MissingTopLevelBindingDecl FilePath Decl'

data SAError
  = ConflictingTypeAnnDeclErr ConflictingTypeAnnDecl
  | ConflictingConstantDeclErr ConflictingConstantDecl
  | ConflictingBindingDeclsErr ConflictingBindingDecls
  | ConflictingArgCountsErr ConflictingArgCounts
  | MissingTopLevelTypeAnnDeclErr MissingTopLevelTypeAnnDecl
  | MissingTopLevelBindingDeclErr MissingTopLevelBindingDecl

newtype SemanticError = SemanticError [SAError]

deriving instance Eq (Decl Parsed) => Eq ConflictingTypeAnnDecl
deriving instance Eq (Binding Parsed) => Eq ConflictingConstantDecl
deriving instance Eq (Binding Parsed) => Eq ConflictingBindingDecls
deriving instance Eq (Binding Parsed) => Eq ConflictingArgCounts
deriving instance Eq (Decl Parsed) => Eq MissingTopLevelTypeAnnDecl
deriving instance Eq (Decl Parsed) => Eq MissingTopLevelBindingDecl
deriving instance Eq (Decl Parsed) => Eq SAError
deriving instance Eq (Decl Parsed) => Eq SemanticError

deriving instance Show (Decl Parsed) => Show ConflictingTypeAnnDecl
deriving instance Show (Binding Parsed) => Show ConflictingConstantDecl
deriving instance Show (Binding Parsed) => Show ConflictingBindingDecls
deriving instance Show (Binding Parsed) => Show ConflictingArgCounts
deriving instance Show (Decl Parsed) => Show MissingTopLevelTypeAnnDecl
deriving instance Show (Decl Parsed) => Show MissingTopLevelBindingDecl
deriving instance Show (Decl Parsed) => Show SAError
deriving instance Show (Decl Parsed) => Show SemanticError

