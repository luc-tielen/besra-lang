
module Besra.SA.ConflictingBindingDecls ( validate ) where

import Protolude
import Besra.SA.Helpers
import Besra.SA.Types
import Besra.Types.IR1.Module
import Besra.Types.IR1.Expr
import Besra.Types.Id
import Besra.Types.Ann


type Module' = Module 'Parsed
type Decl' = Decl 'Parsed

validate :: FilePath -> Validation [SAError] Module'
validate path (Module decls) =
  let typeDecls = filter isBindingDecl decls
      groupedDecls = groupBy sameVar typeDecls
      result = mconcat $ map (checkConflict path) groupedDecls
   in result

isBindingDecl :: Decl' -> Bool
isBindingDecl (BindingDecl _) = True
isBindingDecl _ = False

sameVar :: Decl' -> Decl' -> Bool
sameVar (BindingDecl (Binding _ (Id a) _)) (BindingDecl (Binding _ (Id b) _)) = a == b
sameVar _ _ = False

checkConflict :: FilePath -> [Decl'] -> ValidationResult [SAError]
checkConflict _ [] = Ok
checkConflict _ [_] = Ok
checkConflict path conflicts = Err [err conflicts] where
  err = ConflictingBindingDeclErr . ConflictingBindingDecl path

