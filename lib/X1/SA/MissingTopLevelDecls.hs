
module X1.SA.MissingTopLevelDecls ( validate ) where

import Protolude
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Id
import X1.Types.Ann
import X1.Types.IR1.Module
import X1.Types.IR1.Expr ( Binding(..) )
import X1.Types.IR1.TypeAnn
import qualified Data.List as List


type Module' = Module 'Parsed
type Decl' = Decl 'Parsed

validate :: FilePath -> Validation [SAError] Module'
validate path (Module decls) =
  let matchingDecls = filter isBindingOrTypeAnnDecl decls
      groupedDecls = groupBy sameVar matchingDecls
      result = mconcat $ map (checkConflict path) groupedDecls
   in result

isBindingOrTypeAnnDecl :: Decl' -> Bool
isBindingOrTypeAnnDecl decl = isBindingDecl decl || isTypeAnnDecl decl

isBindingDecl :: Decl' -> Bool
isBindingDecl (BindingDecl _) = True
isBindingDecl _ = False

isTypeAnnDecl :: Decl' -> Bool
isTypeAnnDecl (TypeAnnDecl _) = True
isTypeAnnDecl _ = False

sameVar :: Decl' -> Decl' -> Bool
sameVar (TypeAnnDecl (TypeAnn _ (Id a) _)) (TypeAnnDecl (TypeAnn _ (Id b) _)) = a == b
sameVar (BindingDecl (Binding _ (Id a) _)) (TypeAnnDecl (TypeAnn _ (Id b) _)) = a == b
sameVar (TypeAnnDecl (TypeAnn _ (Id a) _)) (BindingDecl (Binding _ (Id b) _)) = a == b
sameVar (BindingDecl (Binding _ (Id a) _)) (BindingDecl (Binding _ (Id b) _)) = a == b
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

