module X1.SA.Types ( ConflictingTypeDecl(..), SAError(..) ) where

import Protolude
import X1.Types.Module


data ConflictingTypeDecl = ConflictingTypeDecl FilePath [Decl]
  deriving (Eq, Show)

newtype SAError = ConflictingTypeDeclErr ConflictingTypeDecl
  deriving (Eq, Show)
