
module Besra.SA.MissingTopLevelDecls ( validate ) where

import Protolude
import Besra.SA.Helpers
import Besra.SA.Types
import Besra.Types.Id
import Besra.Types.Ann
import qualified Besra.Types.IR1 as IR1
import qualified Data.Map as Map


type Module' = IR1.Module Parsed
type Binding' = IR1.Binding Parsed
type TypeAnn' = IR1.TypeAnn Parsed

data BindingOrTypeAnn
  = B Binding'
  | TA TypeAnn'

validate :: FilePath -> Validation [SAError] Module'
validate path (IR1.Module decls) =
  let relevantDecls = mapMaybe f decls
      f = \case
        IR1.BindingDecl b -> Just (B b)
        IR1.TypeAnnDecl ta -> Just (TA ta)
        _ -> Nothing
   in checkConflicts path relevantDecls

declName :: BindingOrTypeAnn -> Id
declName = \case
  B (IR1.Binding _ name _) -> name
  TA (IR1.TypeAnn _ name _) -> name

checkConflicts :: FilePath -> [BindingOrTypeAnn] -> ValidationResult [SAError]
checkConflicts path decls = foldMap g $ foldr' f Map.empty decls
  where
    f decl = case decl of
      B b -> Map.alter (addBinding $ Just b) (declName decl)
      TA ta -> Map.alter (addTypeAnn $ Just ta) (declName decl)
    addTypeAnn ta = \case
      Nothing -> Just (Nothing, ta)
      Just (b, _) -> Just (b, ta)
    addBinding b = \case
      Nothing -> Just (b, Nothing)
      Just (_, ta) -> Just (b, ta)
    g = \case
      (Just _, Just _) -> Ok
      (Nothing, Nothing) -> Ok
      (Just b, Nothing) -> Err [missingTypeAnn b]
      (Nothing, Just ta) -> Err [missingBinding ta]
    missingTypeAnn = MissingTopLevelTypeAnnDeclErr . MissingTopLevelTypeAnnDecl path
    missingBinding = MissingTopLevelBindingDeclErr . MissingTopLevelBindingDecl path

