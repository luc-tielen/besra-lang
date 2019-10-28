
module Besra.SA.ConflictingTypeAnnDecls ( validate ) where

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
  let typeDecls = filter isTypeAnnDecl decls
      groupedDecls = groupBy sameVar typeDecls
      result = mconcat $ map (checkConflict path) groupedDecls
   in result

isTypeAnnDecl :: Decl' -> Bool
isTypeAnnDecl (IR1.TypeAnnDecl _) = True
isTypeAnnDecl _ = False

sameVar :: Decl' -> Decl' -> Bool
sameVar (IR1.TypeAnnDecl (IR1.TypeAnn _ (Id a) _)) (IR1.TypeAnnDecl (IR1.TypeAnn _ (Id b) _)) = a == b
sameVar _ _ = False

checkConflict :: FilePath -> [Decl'] -> ValidationResult [SAError]
checkConflict _ [] = Ok
checkConflict _ [_] = Ok
checkConflict path conflicts = Err [err conflicts] where
  err = ConflictingTypeAnnDeclErr . ConflictingTypeAnnDecl path

