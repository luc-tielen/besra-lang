
module X1.SA.MissingTopLevelTypeDecls ( validate ) where

--import Protolude
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Module


validate :: Validation SAError (Module Decl)
validate _ = Ok
