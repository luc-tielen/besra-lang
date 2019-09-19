
module Besra.SA.ConflictingBindingDecls ( validate ) where

import Protolude
import Besra.SA.Helpers
import Besra.SA.Types
import qualified Besra.Types.IR1 as IR1
import Besra.Types.Id
import Besra.Types.Ann


type Module' = IR1.Module Parsed
type Decl' = IR1.Decl Parsed

validate :: FilePath -> Validation [SAError] Module'
validate path (IR1.Module decls) =
  let typeDecls = filter isBindingDecl decls
      groupedDecls = groupBy sameVar typeDecls
      result = mconcat $ map (checkConflict path) groupedDecls
   in result

isBindingDecl :: Decl' -> Bool
isBindingDecl (IR1.BindingDecl _) = True
isBindingDecl _ = False

sameVar :: Decl' -> Decl' -> Bool
sameVar (IR1.BindingDecl (IR1.Binding _ (Id a) _)) (IR1.BindingDecl (IR1.Binding _ (Id b) _)) = a == b
sameVar _ _ = False

checkConflict :: FilePath -> [Decl'] -> ValidationResult [SAError]
checkConflict _ [] = Ok
checkConflict _ [_] = Ok
checkConflict path conflicts = Err [err conflicts] where
  err = ConflictingBindingDeclErr . ConflictingBindingDecl path

