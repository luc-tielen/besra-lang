module X1.SA ( runSA, analyze, Validation, ValidationResult(..) ) where

import Protolude
import X1.Types.Module
import X1.SA.Helpers
import X1.SA.Types
import qualified X1.SA.ConflictingTypeDecls as ConflictingTypeDecls
import qualified X1.SA.ConflictingBindingDecls as ConflictingBindingDecls
import qualified X1.SA.MissingTopLevelDecls as MissingTopLevelDecls


validations :: FilePath -> [Validation [SAError] (Module Decl)]
validations path =
  map (\f -> f path)
    [ ConflictingBindingDecls.validate
    , ConflictingTypeDecls.validate
    , MissingTopLevelDecls.validate
    ]

runSA :: FilePath -> Module Decl -> ValidationResult [SAError]
runSA path = analyze (validations path)

