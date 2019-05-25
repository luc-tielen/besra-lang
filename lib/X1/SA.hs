module X1.SA ( runSA
             , analyze
             , Validation
             , ValidationResult(..)
             , SAError(..)
             , SemanticError(..)
             ) where

import Protolude
import X1.Types.Module
import X1.SA.Helpers
import X1.SA.Types
import qualified X1.SA.ConflictingTypeAnnDecls as ConflictingTypeAnnDecls
import qualified X1.SA.ConflictingBindingDecls as ConflictingBindingDecls
import qualified X1.SA.MissingTopLevelDecls as MissingTopLevelDecls


validations :: FilePath -> [Validation [SAError] (Module Decl)]
validations path =
  map (\f -> f path)
    [ ConflictingBindingDecls.validate
    , ConflictingTypeAnnDecls.validate
    , MissingTopLevelDecls.validate
    ]

runSA :: FilePath -> Module Decl -> ValidationResult SemanticError
runSA path decls = SemanticError <$> analyze (validations path) decls

