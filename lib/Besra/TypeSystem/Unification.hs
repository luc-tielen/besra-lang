
module Besra.TypeSystem.Unification
  ( freshUnificationVar
  , unify
  , substituteType
  ) where

import Protolude hiding ( Type )
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Besra.Types.IR3
import Besra.Types.Ann
import Besra.Types.Kind
import Besra.TypeSystem.Monad
import Besra.TypeSystem.Skolems


type KI = KindInferred
type Type' = Type KI

-- Generates a fresh unification variable
freshUnificationVar :: TypeCheckM Type'
freshUnificationVar = do
  ty <- TUnknown <$> gets nextUnificationVar
  modify $ \s -> s { nextUnificationVar = nextUnificationVar s + 1 }
  pure ty

-- Tries to unify 2 types, updating the Substitution along the way.
unify :: Type' -> Type' -> TypeCheckM ()
unify t1 t2 = addContext (WhileUnifyingTypes t1 t2) $ do
  subst <- gets substitution
  unify' (substituteType subst t1) (substituteType subst t2)
  where
    unify' (TApp t11 t12) (TApp t21 t22) = do
      unify t11 t21
      unify t12 t22
    unify' (TCon c1) (TCon c2) | sameTycon c1 c2 = pure ()
    unify' (TVar v1) (TVar v2) | sameTyvar v1 v2 = pure ()
    unify' (TUnknown u1) (TUnknown u2) | u1 == u2 = pure ()
    unify' (TUnknown u) t = solveType u t
    unify' t (TUnknown u) = solveType u t
    unify' (TForAll ann1 name1 (Just scope1) ty1) (TForAll ann2 name2 (Just scope2) ty2) = do
      skolem <- newSkolemConstant
      let ty1' = replaceVarWithSkolem name1 ann1 skolem scope1 ty1
      let ty2' = replaceVarWithSkolem name2 ann2 skolem scope2 ty2
      unify ty1' ty2'
    unify' TForAll {} TForAll {} = panic "Found unitialized skolem scope."
    unify' (TForAll ann name (Just scope) ty1) ty2 = do
      skolem <- newSkolemConstant
      let ty1' = replaceVarWithSkolem name ann skolem scope ty1
      unify ty1' ty2
    unify' TForAll {} _ = panic "Found unitialized skolem scope."
    unify' ty1 ty2@TForAll {} = unify ty2 ty1
    unify' (TSkolem _ _ _ skolem1) (TSkolem _ _ _ skolem2) | skolem1 == skolem2 = pure ()
    unify' ty1 ty2 = throwError $ UnificationFailure ty1 ty2
    sameTycon (Tycon _ c1) (Tycon _ c2) = c1 == c2
    sameTyvar (Tyvar ann1 v1) (Tyvar ann2 v2) =
      v1 == v2 && kind ann1 == kind ann2

-- Recursively substitute the type given a subtitution
substituteType :: Substitution -> Type' -> Type'
substituteType subst = \case
  TApp t1 t2 -> TApp (substituteType subst t1) (substituteType subst t2)
  TForAll ann var scope t -> TForAll ann var scope (substituteType subst t)
  TUnknown unknown ->
    case IntMap.lookup unknown subst of
      Nothing -> TUnknown unknown
      Just (TUnknown unknown') | unknown == unknown' -> TUnknown unknown'
      Just t -> substituteType subst t
  t -> t

-- Solve the current type and update the Substitution
solveType :: Int -> Type' -> TypeCheckM ()
solveType u t = do
  occursCheck u t
  modify $ \s ->
    let subst = substitution s
     in s { substitution = IntMap.insert u t subst }

occursCheck :: Int -> Type' -> TypeCheckM ()
occursCheck unknown ty =
  when (isJust $ List.find (== unknown) (unknowns ty)) $
    throwError $ OccursCheck (TUnknown unknown) ty
  where unknowns :: Type' -> [Int]
        unknowns = \case
          TUnknown u -> [u]
          TApp t1 t2 -> unknowns t1 <> unknowns t2
          TForAll _ _ _ t -> unknowns t
          _ -> mempty

