
module X1.SA.ConflictingTypeDecls ( validate ) where

import Protolude
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Module
import X1.Types.Id


validate :: FilePath -> Validation [SAError] (Module Decl)
validate path (Module decls) =
  let typeDecls = filter isTypeDecl decls
      groupedDecls = groupBy sameVar typeDecls
      result = mconcat $ map (checkConflict path) groupedDecls
   in result

isTypeDecl :: Decl -> Bool
isTypeDecl (TypeDecl _ _) = True
isTypeDecl _ = False

sameVar :: Decl -> Decl -> Bool
sameVar (TypeDecl (Id a) _) (TypeDecl (Id b) _) = a == b
sameVar _ _ = False

checkConflict :: FilePath -> [Decl] -> ValidationResult [SAError]
checkConflict _ [] = Ok
checkConflict _ [_] = Ok
checkConflict path conflicts = Err [err conflicts] where
  err = ConflictingTypeDeclErr . ConflictingTypeDecl path

