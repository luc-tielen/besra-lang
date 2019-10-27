
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
import qualified Besra.Types.IR1 as IR1
import Besra.Types.Ann

type Decl' = IR1.Decl Parsed
type Binding' = IR1.Binding Parsed

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

deriving instance Eq Decl' => Eq ConflictingTypeAnnDecl
deriving instance Eq Binding' => Eq ConflictingConstantDecl
deriving instance Eq Binding' => Eq ConflictingBindingDecls
deriving instance Eq Binding' => Eq ConflictingArgCounts
deriving instance Eq Decl' => Eq MissingTopLevelTypeAnnDecl
deriving instance Eq Decl' => Eq MissingTopLevelBindingDecl
deriving instance Eq Decl' => Eq SAError
deriving instance Eq Decl' => Eq SemanticError

deriving instance Show Decl' => Show ConflictingTypeAnnDecl
deriving instance Show Binding' => Show ConflictingConstantDecl
deriving instance Show Binding' => Show ConflictingBindingDecls
deriving instance Show Binding' => Show ConflictingArgCounts
deriving instance Show Decl' => Show MissingTopLevelTypeAnnDecl
deriving instance Show Decl' => Show MissingTopLevelBindingDecl
deriving instance Show Decl' => Show SAError
deriving instance Show Decl' => Show SemanticError

