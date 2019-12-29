
module Besra.TypeSystem.Monad
  ( Substitution
  , CheckState(..)
  , TypeCheckM
  , TypeError(..)
  , TypeEnv
  , runTC
  , addContext
  , lookupVariable
  , bindLocalVariables
  , replaceVarWithType
  ) where

import Protolude hiding ( Type, TypeError )
import Besra.Types.IR3
import Besra.Types.Ann
import Besra.Types.Id
import Control.Monad.RWS
import qualified Data.Set as Set
import qualified Data.Map as Map

type KI = KindInferred
type Type' = Type KI
type Expr' = Expr KI

type Substitution = IntMap Type'

type TypeEnv = Map Id Type'

data CheckState
  = CheckState
  { nextUnificationVar :: Int
  , nextSkolemVar :: Int
  , nextSkolemScope :: Int
  , substitution :: Substitution
  } deriving Show

data TypeError
  = TypeMismatch Type' Type'
  | ExpectedArrowType Expr' Type'
  | UnificationFailure Type' Type'
  | UnboundVariable Id
  | OccursCheck Type' Type'
  | EscapedSkolem (Ann KI) Id Type'
  | PatternArityMismatch Id (Type KI) Int Int
  | MultipleErrors (NonEmpty TypeError)
  | WhileChecking Type' Expr' TypeError
  | WhileInferring Expr' TypeError
  | WhileUnifyingTypes Type' Type' TypeError
  deriving (Eq, Show)

type TypeCheckM = ExceptT TypeError (RWS TypeEnv () CheckState)


-- TODO no need to return substitution?
-- | Runs the type checker, with a certain type env.
runTC :: TypeEnv -> TypeCheckM a -> (Substitution, Either TypeError a)
runTC env m =
  let (res, cs, _) = runRWS (runExceptT m) env (CheckState 0 0 0 mempty)
      subst = substitution cs
   in (subst, res)

-- | Adds extra context to a type error.
addContext :: (TypeError -> TypeError) -> TypeCheckM a -> TypeCheckM a
addContext ctx m = catchError m (throwError . ctx)

-- | Looks up a variable in the type environment.
-- Throws an error if the environment does not contain a variable with the given name.
lookupVariable :: Id -> TypeCheckM Type'
lookupVariable var = asks (Map.lookup var) >>= \case
  Nothing -> throwError $ UnboundVariable var
  Just ty -> pure ty

-- | Binds local type variables for the duration of an action.
bindLocalVariables :: TypeEnv -> TypeCheckM a -> TypeCheckM a
bindLocalVariables vars = local (<> vars)

-- | Replaces a variable with a type.
--   This function takes name shadowing introduced by foralls into account.
replaceVarWithType :: Id -> Type' -> (Type' -> Type')
replaceVarWithType var replacement t = runReader (go t) Set.empty where
  go = \case
    TVar v@(Tyvar _ var') | var == var' -> do
      skolemsInScope <- ask
      pure $ if var `elem` skolemsInScope then TVar v else replacement
    TForAll ann var' scope' ty -> local (Set.insert var') $
      TForAll ann var' scope' <$> go ty
    TApp t1 t2 -> TApp <$> go t1 <*> go t2
    ty -> pure ty

