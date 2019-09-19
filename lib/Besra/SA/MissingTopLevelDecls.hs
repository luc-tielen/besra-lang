
module Besra.SA.MissingTopLevelDecls ( validate ) where

import Protolude
import Besra.SA.Helpers
import Besra.SA.Types
import Besra.Types.Id
import Besra.Types.Ann
import qualified Besra.Types.IR1 as IR1
import qualified Data.List as List


type Module' = IR1.Module Parsed
type Decl' = IR1.Decl Parsed

validate :: FilePath -> Validation [SAError] Module'
validate path (IR1.Module decls) =
  let matchingDecls = filter isBindingOrTypeAnnDecl decls
      groupedDecls = groupBy sameVar matchingDecls
      result = mconcat $ map (checkConflict path) groupedDecls
   in result

isBindingOrTypeAnnDecl :: Decl' -> Bool
isBindingOrTypeAnnDecl decl = isBindingDecl decl || isTypeAnnDecl decl

isBindingDecl :: Decl' -> Bool
isBindingDecl (IR1.BindingDecl _) = True
isBindingDecl _ = False

isTypeAnnDecl :: Decl' -> Bool
isTypeAnnDecl (IR1.TypeAnnDecl _) = True
isTypeAnnDecl _ = False

sameVar :: Decl' -> Decl' -> Bool
sameVar (IR1.TypeAnnDecl (IR1.TypeAnn _ (Id a) _)) (IR1.TypeAnnDecl (IR1.TypeAnn _ (Id b) _)) = a == b
sameVar (IR1.BindingDecl (IR1.Binding _ (Id a) _)) (IR1.TypeAnnDecl (IR1.TypeAnn _ (Id b) _)) = a == b
sameVar (IR1.TypeAnnDecl (IR1.TypeAnn _ (Id a) _)) (IR1.BindingDecl (IR1.Binding _ (Id b) _)) = a == b
sameVar (IR1.BindingDecl (IR1.Binding _ (Id a) _)) (IR1.BindingDecl (IR1.Binding _ (Id b) _)) = a == b
sameVar _ _ = False

checkConflict :: FilePath -> [Decl'] -> ValidationResult [SAError]
checkConflict path decls =
  let maybeTypeAnnDecl = List.find isTypeAnnDecl decls
      maybeBindingDecl = List.find isBindingDecl decls
      missingType = MissingTopLevelTypeAnnDeclErr . MissingTopLevelTypeAnnDecl path
      missingBinding = MissingTopLevelBindingDeclErr . MissingTopLevelBindingDecl path
      result = case (maybeTypeAnnDecl, maybeBindingDecl) of
        (Just _, Just _) -> Ok
        (Nothing, Nothing) -> Ok
        (Just ty, Nothing) -> Err [missingBinding ty]
        (Nothing, Just binding) -> Err [missingType binding]
     in result

