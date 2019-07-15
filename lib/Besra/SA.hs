module Besra.SA ( runSA
             , analyze
             , Validation
             , ValidationResult(..)
             , SAError(..)
             , SemanticError(..)
             ) where

import Protolude
import Besra.Types.IR1.Module
import Besra.SA.Helpers
import Besra.SA.Types
import Besra.Types.Ann
import qualified Besra.SA.ConflictingTypeAnnDecls as ConflictingTypeAnnDecls
import qualified Besra.SA.ConflictingBindingDecls as ConflictingBindingDecls
import qualified Besra.SA.MissingTopLevelDecls as MissingTopLevelDecls


type Module' = Module 'Parsed

validations :: FilePath -> [Validation [SAError] Module']
validations path =
  map (\f -> f path)
    [ ConflictingBindingDecls.validate
    , ConflictingTypeAnnDecls.validate
    , MissingTopLevelDecls.validate
    ]

runSA :: FilePath -> Module' -> ValidationResult SemanticError
runSA path decls = SemanticError <$> analyze (validations path) decls

