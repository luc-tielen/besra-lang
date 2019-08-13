
module Besra.TypeSystem.KindSolver
  ( KindEnv
  , KindError(..)
  , KAssump
  , KConstraint(..)
  , KSubst
  , Substitutable(..)
  , Infer
  , IKind(..)
  , runInfer
  , fresh -- TODO dont export
  , infer
  , solve
  , solveConstraints
  , sameVarConstraints
  , normalizeKind
  , toIKind
  , mkKindEnv
  ) where

import Protolude hiding ( Type, show )
import Prelude ( Show(..) )
import Unsafe ( unsafeHead )
import Control.Monad.RWS.Strict
import Besra.Types.IR2.Type
import Besra.Types.Kind
import Besra.Types.Ann
import Besra.Types.Id
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map ( Map )
import qualified Data.Text as T


-- | Data type used internally for representing equations of kinds.
--   Compared to the normal kind type, this also contains a variable that
--   is used during the unification algorithm.
data IKind = IStar
           | IKArr IKind IKind
           | IKVar Id
           deriving (Eq, Ord)

-- TODO remove, not needed?
instance Show IKind where
  show IStar         = "*"
  show (IKArr k1 k2) = "(" <> show k1 <> " -> " <> show k2 <> ")"
  show (IKVar var)   = show var

-- | Function for getting the actual kind from an inferred kind.
--   This defaults kinds of remaining phantom type variables to *
normalizeKind :: IKind -> Kind
normalizeKind = \case
  IStar -> Star
  IKArr k1 k2 -> KArr (normalizeKind k1) (normalizeKind k2)
  IKVar _ -> Star

toIKind :: Kind -> IKind
toIKind = \case
  Star -> IStar
  KArr k1 k2 -> IKArr (toIKind k1) (toIKind k2)

data KConstraint = KConstraint IKind IKind
  deriving Eq

-- TODO remove, not needed?
instance Show KConstraint where
  show (KConstraint k1 k2) = show k1 <> " ~ " <> show k2

type Counter = Int

type KAssump = (Id, IKind)

type KindEnv = Map Id IKind

-- TODO add spans
data KindError = UnificationFail IKind IKind
               | InfiniteKind Id IKind
  deriving (Eq, Show)

type Infer = RWST KindEnv () Counter (Except KindError)

newtype KSubst = KSubst (Map Id IKind)
  deriving (Eq, Show)

instance Semigroup KSubst where
  s1 <> s2 = substitute s1 s2 `union` s1
    where union (KSubst a) (KSubst b) = KSubst (a `Map.union` b)

instance Monoid KSubst where
  mempty = KSubst mempty


class Substitutable a where
  substitute :: KSubst -> a -> a

instance Substitutable a => Substitutable [a] where
  substitute = map . substitute

instance Substitutable KSubst where
  substitute s1 (KSubst m) = KSubst $ map (substitute s1) m

instance Substitutable IKind where
  substitute s@(KSubst subst) = \case
    IStar -> IStar
    IKArr k1 k2 -> IKArr (substitute s k1) (substitute s k2)
    k@(IKVar kv) -> Map.findWithDefault k kv subst

instance Substitutable KConstraint where
  substitute s (KConstraint k1 k2) =
    KConstraint (substitute s k1) (substitute s k2)


runInfer :: Infer a -> KindEnv -> Either KindError a
runInfer m env = fst <$> runExcept (evalRWST m env 0)

solveConstraints :: [Type 'Parsed] -> Infer KindEnv
solveConstraints ts = do
  results <- traverse infer ts
  let combine :: Monoid a => (([KAssump], [KConstraint], IKind) -> a) -> a
      combine f = foldMap f results
      as = combine (\(as', _, _) -> as')
      cs = combine (\(_, cs', _) -> cs')
      constraints = cs <> sameVarConstraints as
  subst <- solve constraints
  pure $ mkKindEnv as subst

-- | Since we have multiple equations all with different variables,
--   generate extra constraints making them equal
sameVarConstraints :: [KAssump] -> [KConstraint]
sameVarConstraints as =
  let groupById = List.groupBy ((==) `on` fst) . List.sortOn fst
      groupedAs = groupById as
      groupedEquations = map (map snd) groupedAs
      getCombinations (x:xs) = [(x, y) | y <- xs, x /= y]
      getCombinations _ = []
      groupedCs = map getCombinations groupedEquations
   in uncurry KConstraint <$> mconcat groupedCs

-- | Infers the kind of an expression at the type level
infer :: Type 'Parsed -> Infer ([KAssump], [KConstraint], IKind)
infer = \case
  TVar (Tyvar _ varName) -> do
    kv <- IKVar <$> fresh
    pure ([(varName, kv)], mempty, kv)
  TCon (Tycon _ con) -> do
    kv <- IKVar <$> fresh
    maybeK <- asks (Map.lookup con)
    let cs = maybe mempty (\k -> [KConstraint kv k]) maybeK
    pure ([(con, kv)], cs, kv)
  TApp f arg -> do
    (as1, cs1, k1) <- infer f
    (as2, cs2, k2) <- infer arg
    kv <- IKVar <$> fresh
    let cs = cs1 <> cs2 <> [KConstraint k1 (IKArr k2 kv)]
    pure (as1 <> as2, cs, kv)

fresh :: Infer Id
fresh = do
  ctr <- get
  modify (+ 1)
  pure . Id $ "k" <> T.pack (show ctr)


solve :: [KConstraint] -> Infer KSubst
solve [] = pure mempty
solve (KConstraint k1 k2 : cs) = do
  su1 <- unify k1 k2
  su2 <- solve $ substitute su1 cs
  pure $ su2 <> su1

unify :: IKind -> IKind -> Infer KSubst
unify k1 k2 | k1 == k2 = pure mempty
unify (IKVar v) k = v `bindTo` k
unify k (IKVar v) = v `bindTo` k
unify (IKArr k1 k2) (IKArr k3 k4) = do
  su1 <- unify k1 k3
  su2 <- unify (substitute su1 k2) (substitute su1 k4)
  pure $ su2 <> su1
unify k1 k2 = throwError $ UnificationFail k1 k2

bindTo :: Id -> IKind -> Infer KSubst
bindTo kv k
  | k == IKVar kv = pure mempty
  | occursCheck kv k = throwError $ InfiniteKind kv k
  | otherwise = pure . KSubst $ Map.fromList [(kv, k)]

occursCheck :: Id -> IKind -> Bool
occursCheck kv k = kv `elem` kindVars where
  kindVars = getVars k
  getVars = \case
    IKArr k1 k2 -> getVars k1 ++ getVars k2
    IKVar v -> [v]
    IStar -> []

mkKindEnv :: [KAssump] -> KSubst -> KindEnv
mkKindEnv as subst =
  let trimmedAs = uniq as
      filledInAs = [(var, substitute subst k) | (var, k) <- trimmedAs]
  in Map.fromList filledInAs

uniq :: Ord a => [a] -> [a]
uniq = map unsafeHead . group . sort

