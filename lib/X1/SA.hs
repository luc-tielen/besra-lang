module X1.SA ( runSA
             , analyze
             , Validation
             , ValidationResult(..)
             , SAError(..)
             , SemanticError(..)
             ) where

import Protolude
import X1.Types.IR1.Module
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Ann
import qualified X1.SA.ConflictingTypeAnnDecls as ConflictingTypeAnnDecls
import qualified X1.SA.ConflictingBindingDecls as ConflictingBindingDecls
import qualified X1.SA.MissingTopLevelDecls as MissingTopLevelDecls


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

