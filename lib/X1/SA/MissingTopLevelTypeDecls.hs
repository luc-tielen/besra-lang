
module X1.SA.MissingTopLevelTypeDecls ( validate ) where

import Protolude
import X1.SA.Helpers
import X1.SA.Types
import X1.Types.Module
import X1.Types.Id
import qualified Data.List as List


validate :: FilePath -> Validation [SAError] (Module Decl)
validate path (Module decls) =
  let matchingDecls = filter isBindingOrTypeDecl decls
      groupedDecls = groupBy sameVar matchingDecls
      result = mconcat $ map (checkConflict path) groupedDecls
   in result

isBindingOrTypeDecl :: Decl -> Bool
isBindingOrTypeDecl decl = isBindingDecl decl || isTypeDecl decl

isBindingDecl :: Decl -> Bool
isBindingDecl (BindingDecl _ _) = True
isBindingDecl _ = False

isTypeDecl :: Decl -> Bool
isTypeDecl (TypeDecl _ _) = True
isTypeDecl _ = False

sameVar :: Decl -> Decl -> Bool
sameVar (TypeDecl (Id a) _) (TypeDecl (Id b) _) = a == b
sameVar (BindingDecl (Id a) _) (TypeDecl (Id b) _) = a == b
sameVar (TypeDecl (Id a) _) (BindingDecl (Id b) _) = a == b
sameVar (BindingDecl (Id a) _) (BindingDecl (Id b) _) = a == b

checkConflict :: FilePath -> [Decl] -> ValidationResult [SAError]
checkConflict path decls =
  let maybeTypeDecl = List.find isTypeDecl decls
      maybeBindingDecl = List.find isBindingDecl decls
      err = MissingTopLevelTypeDeclErr . MissingTopLevelTypeDecl path
      result = case (maybeTypeDecl, maybeBindingDecl) of
        (Just _ty, Just _binding) -> Ok
        (Just _, Nothing) -> Err []  -- TODO missing top level binding <== fuse checks together!
        (Nothing, Just binding) -> Err [err binding]
        (Nothing, Nothing) -> Ok
     in result

