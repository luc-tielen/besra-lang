
module X1.SA.ConflictingBindingDecls ( validate ) where

import Protolude
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Module
import X1.Types.Expr1
import X1.Types.Id


validate :: FilePath -> Validation [SAError] (Module Decl)
validate path (Module decls) =
  let typeDecls = filter isBindingDecl decls
      groupedDecls = groupBy sameVar typeDecls
      result = mconcat $ map (checkConflict path) groupedDecls
   in result

isBindingDecl :: Decl -> Bool
isBindingDecl (BindingDecl _) = True
isBindingDecl _ = False

sameVar :: Decl -> Decl -> Bool
sameVar (BindingDecl (Binding (Id a) _)) (BindingDecl (Binding (Id b) _)) = a == b
sameVar _ _ = False

checkConflict :: FilePath -> [Decl] -> ValidationResult [SAError]
checkConflict _ [] = Ok
checkConflict _ [_] = Ok
checkConflict path conflicts = Err [err conflicts] where
  err = ConflictingBindingDeclErr . ConflictingBindingDecl path

