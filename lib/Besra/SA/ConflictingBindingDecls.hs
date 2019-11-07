
module Besra.SA.ConflictingBindingDecls ( validate ) where

import Protolude
import Unsafe ( unsafeHead )
import Control.Arrow ( (&&&) )
import Besra.SA.Helpers
import Besra.SA.Types
import Besra.Types.IR1
import Besra.Types.Ann
import Besra.Types.Id
import qualified Data.List as List
import qualified Data.Map as Map


type Module' = Module Parsed
type Binding' = Binding Parsed

data DeclType
  = BindingType
  | OtherType
  deriving Eq


validate :: FilePath -> Validation [SAError] Module'
validate path (Module decls) =
  let groupedDecls = toGroupedDecls decls
      groupedBindings = toBindings groupedDecls
   in checkConflict path groupedBindings

toGroupedDecls :: [Decl ph] -> [[Decl ph]]
toGroupedDecls = List.groupBy ((==) `on` typeOfDecl &&& bindingDeclName) where
  typeOfDecl = withBinding (const BindingType) OtherType
  bindingDeclName = withBinding (Just . bindingName) Nothing

toBindings :: [[Decl ph]] -> [[Binding ph]]
toBindings = filter (not . null) . map (mapMaybe toMaybeBinding) where
  toMaybeBinding = withBinding Just Nothing

checkConflict :: FilePath -> [[Binding']] -> ValidationResult [SAError]
checkConflict path =
  let conflictsPerGroup = foldMap (checkConflictsPerGroup path)
      overallConflicts = checkOverallConflicts path
   in conflictsPerGroup <> overallConflicts

checkConflictsPerGroup :: FilePath -> [Binding'] -> ValidationResult [SAError]
checkConflictsPerGroup path = \case
  [] -> Ok
  [_] -> Ok
  bindings' ->
    if | all (== 0) argCounts -> constantErr path bindings'
       | minimum argCounts /= maximum argCounts -> argCountErr path bindings'
       | otherwise  -> Ok
    where argCounts = map argCount bindings'

checkOverallConflicts :: FilePath -> [[Binding']] -> ValidationResult [SAError]
checkOverallConflicts path groupedBindings =
  let nameForGroup = bindingName . unsafeHead
      bindings' = map (nameForGroup &&& identity) groupedBindings
      f (!result, !bindingsMap) (!name, !bindings) =
        case Map.lookup name bindingsMap of
          Just bindings'' ->
            (result <> conflictErr path bindings'' bindings, bindingsMap)
          Nothing ->
            (result, Map.insert name bindings bindingsMap)
   in fst $ foldl' f (Ok, Map.empty) bindings'


-- Helper functions:

constantErr :: FilePath -> [Binding'] -> ValidationResult [SAError]
constantErr path bindings =
  Err [ConflictingConstantDeclErr $ ConflictingConstantDecl path bindings]

argCountErr :: FilePath -> [Binding'] -> ValidationResult [SAError]
argCountErr path bindings =
  Err [ConflictingArgCountsErr $ ConflictingArgCounts path bindings]

conflictErr :: FilePath -> [Binding'] -> [Binding'] -> ValidationResult [SAError]
conflictErr path bs1 bs2 =
  Err [ConflictingBindingDeclsErr $ ConflictingBindingDecls path bs1 bs2]

withBinding :: (Binding ph -> a) -> a -> Decl ph -> a
withBinding f def = \case
  BindingDecl binding -> f binding
  _ -> def

bindingName :: Binding ph -> Id
bindingName (Binding _ name _) = name

-- NOTE: does not take pointfree style into account
class ArgCount a where
  argCount :: a -> Int

instance ArgCount (Binding ph) where
  argCount (Binding _ _ expr) = argCount expr

instance ArgCount (Expr ph) where
  argCount = \case
    ELam _ pats e -> length pats + argCount e
    _ -> 0

