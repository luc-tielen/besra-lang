
module X1.SA.ConflictingTypeAnnDecls ( validate ) where

import Protolude
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Expr1.Module
import X1.Types.Expr1.TypeAnn
import X1.Types.Id


validate :: FilePath -> Validation [SAError] Module
validate path (Module decls) =
  let typeDecls = filter isTypeAnnDecl decls
      groupedDecls = groupBy sameVar typeDecls
      result = mconcat $ map (checkConflict path) groupedDecls
   in result

isTypeAnnDecl :: Decl -> Bool
isTypeAnnDecl (TypeAnnDecl _) = True
isTypeAnnDecl _ = False

sameVar :: Decl -> Decl -> Bool
sameVar (TypeAnnDecl (TypeAnn (Id a) _)) (TypeAnnDecl (TypeAnn (Id b) _)) = a == b
sameVar _ _ = False

checkConflict :: FilePath -> [Decl] -> ValidationResult [SAError]
checkConflict _ [] = Ok
checkConflict _ [_] = Ok
checkConflict path conflicts = Err [err conflicts] where
  err = ConflictingTypeAnnDeclErr . ConflictingTypeAnnDecl path

