
{-# LANGUAGE UndecidableInstances #-}

module Besra.Pass.InferKinds
  ( pass
  , CompilerState(..)
  , PredKindEnv
  , KEnv(..)
  ) where

import Protolude hiding ( Type, pass, show )
import Unsafe ( unsafeFromJust )
import Data.Graph
import Besra.TypeSystem.KindSolver
import Besra.Types.IR2
import Besra.Types.Ann
import Besra.Types.Id
import Besra.Types.Kind
import qualified Data.Vector.Unboxed as VU
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map ( Map )


-- The complete documentation for this algorithm can be found here:
-- https://github.com/luc-tielen/besra-lang/blob/master/docs/algorithms/kind_inference.md

-- | Data type for keeping track of mapping for traits and the corresponding
--   kinds for each of the type variables in a trait.
type PredKindEnv = Map Id [IKind]

data KEnv = KEnv
          { kEnv :: KindEnv
          , kPredEnv :: PredKindEnv
          } deriving (Eq, Show)

-- TODO consistent naming of types?
-- TODO move to other file
data CompilerState (ph :: Phase)
  = CompilerState [ADT ph] [Trait ph] [Impl ph] KEnv

deriving instance AnnHas Eq ph => Eq (CompilerState ph)
deriving instance AnnHas Show ph => Show (CompilerState ph)

-- TODO merge KI with Infer (and use RWS KEnv ...)
type KI = ReaderT PredKindEnv Infer


pass :: MonadError KindError m
     => CompilerState 'Parsed
     -> Module 'Parsed
     -> m (Module 'KindInferred, CompilerState 'KindInferred)
pass (CompilerState adts traits impls kEnv) ast =
  flip evalStateT kEnv $ do
    adts' <- inferADTs adts
    traits' <- inferTraits traits
    kEnv' <- get
    let result = flip runKI kEnv' $ do
          impls' <- solveKinds impls
          ast' <- solveKinds ast
          pure (impls', ast')
    case result of
      Left err -> throwError err
      Right (impls', ast') ->
        pure (ast', CompilerState adts' traits' impls' kEnv')

runKI :: KI a -> KEnv -> Either KindError a
runKI m (KEnv kindEnv predEnv) =
  flip runInfer kindEnv $ runReaderT m predEnv

inferADTs :: (MonadError KindError m, MonadState KEnv m)
          => [ADT 'Parsed]
          -> m [ADT 'KindInferred]
inferADTs adts = concatMapM inferADTGroup groupedADTs where
  adtsGraph = map (\adt -> (adt, adtName adt, adtRefersTo adt)) adts
  groupedADTs = graphToGroupedLists adtsGraph
  updateState (solution, adts') = do
    let solution' = Map.filterWithKey (\k _ -> isNoVar k) solution
    KEnv oldEnv predEnv <- get
    let newEnv = oldEnv <> (normalizeIKind <$> solution')
    put (KEnv newEnv predEnv) $> adts'
  inferADTGroup adtGroup = do
    env <- gets kEnv
    let result = runInfer (inferKindForADTs adtGroup) env
    either throwError updateState result
  isNoVar (Id x) = not $ T.head x `VU.elem` ['a'..'z']

inferKindForADTs :: [ADT 'Parsed] -> Infer (KindEnv, [ADT 'KindInferred])
inferKindForADTs adts = do
  let types = concatMap getTypeEquations adts
  updatedKindEnv <- solveADTConstraints types
  let adts' = map (enrichADT updatedKindEnv) adts
  pure (updatedKindEnv, adts')

solveADTConstraints :: [Type 'Parsed] -> Infer KindEnv
solveADTConstraints ts = do
  results <- traverse infer ts
  let (as, cs) = gatherResults results
      constraints = cs <> sameVarConstraints as
  subst <- solve constraints
  pure $ mkKindEnv as subst

inferTraits :: (MonadError KindError m, MonadState KEnv m)
            => [Trait 'Parsed]
            -> m [Trait 'KindInferred]
inferTraits traits = mapM inferTrait orderedTraits where
  traitsGraph = map (\t -> (t, traitName t, traitRefersTo t)) traits
  orderedTraits = mconcat $ graphToGroupedLists traitsGraph
  updateState (solution, trait') = do
    KEnv env _ <- get
    let newPredEnv = map normalizeIKind <$> solution
    put (KEnv env newPredEnv) $> trait'
  inferTrait trait = do
    env <- get
    let result = runKI (inferKindForTrait trait) env
    either throwError updateState result

inferKindForTrait :: Trait 'Parsed
                  -> KI (PredKindEnv, Trait 'KindInferred)
inferKindForTrait (Trait sp ps p@(IsIn _ predName predTys) tys) = do
  predEnv <- ask
  predResults <- lift $ traverse infer predTys
  let (predAs, predCs) = gatherResults predResults
  results <- mapM gatherTraitTypeConstraints tys
  headResults <- traverse (lift . inferPred predEnv) ps
  let (as, cs) = gatherResults results
      (headAs, headCs) = gatherResults headResults
      assumps = as <> headAs <> predAs
      bodyCs = cs <> sameVarConstraints assumps
      constraints = predCs <> headCs <> bodyCs
  subst <- lift $ solve constraints
  let kindEnv = mkKindEnv assumps subst
      predVars = getPredVars p
      predEnv' = predEnv <> Map.fromList [(predName, map (\v -> unsafeFromJust $ Map.lookup v kindEnv) predVars)]
      ps' = map (lookupKindsForPred predEnv') ps
      p' = lookupKindsForPred predEnv' p
  tys' <- local (const predEnv') $ solveKinds tys
  let trait = Trait sp ps' p' tys'
  pure (predEnv', trait)

gatherTraitTypeConstraints :: TypeAnn 'Parsed -> KI ([KAssump], [KConstraint])
gatherTraitTypeConstraints (TypeAnn _ _ sch) = do
  predEnv <- ask
  let Scheme _ ps ty = sch
  predResults <- traverse (lift . inferPred predEnv) ps
  let (predAs, predCs) = gatherResults predResults
  (as, cs, k) <- lift $ infer ty
  let constraints = KConstraint IStar k : cs <> predCs
  pure (as <> predAs, constraints)

lookupKindsForPred :: PredKindEnv -> Pred 'Parsed -> Pred 'KindInferred
lookupKindsForPred predEnv p@(IsIn ann name tys) = IsIn ann name tys' where
  ks = lookupPred p predEnv
  tys' = zipWith f tys ks
  f (TVar (Tyvar sp var)) k = TVar (Tyvar (sp, normalizeKind k) var)
  f _ _ = panic "Not implemented!" -- TODO

class HasType a ph where
  getType :: a -> Type ph

instance HasType (ADTHead ph) ph where
  getType (ADTHead _ ty) = ty

instance HasType (ConDecl ph) ph where
  getType (ConDecl _ _ ty) = ty

-- | Helper function for extracting all the types used in the ADT.
getTypeEquations :: ADT 'Parsed -> [Type 'Parsed]
getTypeEquations (ADT _ hd bodies) =
  let hdType = getType hd
      bodyTypes = getType <$> bodies
   in hdType:bodyTypes


class SolveKinds a where
  type Result a

  solveKinds :: a -> KI (Result a)

instance SolveKinds a => SolveKinds [a] where
  type Result [a] = [Result a]

  solveKinds = traverse solveKinds

instance SolveKinds (Module 'Parsed) where
  type Result (Module 'Parsed) = Module 'KindInferred

  solveKinds (Module decls) =
    Module <$> solveKinds decls

instance SolveKinds (Decl 'Parsed) where
  type Result (Decl 'Parsed) = Decl 'KindInferred

  solveKinds = \case
    TypeAnnDecl t -> TypeAnnDecl <$> solveKinds t
    BindingDecl b -> BindingDecl <$> solveKinds b

instance SolveKinds (Binding 'Parsed) where
  type Result (Binding 'Parsed) = Binding 'KindInferred

  solveKinds (Binding ann name e) =
    Binding ann name <$> solveKinds e

instance SolveKinds (Expr 'Parsed) where
  type Result (Expr 'Parsed) = Expr 'KindInferred

  solveKinds = \case
    ELit sp lit -> pure $ ELit sp lit
    EVar sp var -> pure $ EVar sp var
    ECon sp con -> pure $ ECon sp con
    ELam sp args body -> ELam sp args <$> solveKinds body
    EApp sp f arg ->
      EApp sp <$> solveKinds f <*> solveKinds arg
    EIf sp cnd tr fl ->
      EIf sp <$> solveKinds cnd <*> solveKinds tr <*> solveKinds fl
    ECase sp expr clauses ->
      ECase sp <$> solveKinds expr <*> traverse (traverse solveKinds) clauses
    ELet sp decls body ->
      ELet sp <$> traverse solveKinds decls <*> solveKinds body

instance SolveKinds (TypeAnn 'Parsed) where
  type Result (TypeAnn 'Parsed) = TypeAnn 'KindInferred

  solveKinds t@(TypeAnn _ _ sch) = do
    predEnv <- ask
    let Scheme _ ps ty = sch
    predResults <- traverse (lift . inferPred predEnv) ps
    let (predAs, predCs) = gatherResults predResults
    (as, cs, k) <- lift $ infer ty
    let assumps = as <> predAs
        constraints = KConstraint IStar k : sameVarConstraints assumps <> cs <> predCs
    subst <- lift $ solve constraints
    let kindEnv = mkKindEnv assumps subst
    pure $ runReader (enrich t) kindEnv

instance SolveKinds (Impl 'Parsed) where
  type Result (Impl 'Parsed) = Impl 'KindInferred

  solveKinds (Impl ann ps p@(IsIn _ _ tys) bindings) = do
    predEnv <- ask
    implResults <- lift $ traverse infer tys
    predResults <- traverse (lift . inferPred predEnv) ps
    let (implAs, implCs) = gatherResults implResults
        implKs = map (\(_, _, k) -> k) implResults
        (predAs, predCs) = gatherResults predResults
        predKs = lookupPred p predEnv
        predCs' = zipWith KConstraint implKs predKs
        assumps = implAs <> predAs
        constraints = implCs <> predCs <> predCs' <> sameVarConstraints assumps
    subst <- lift $ solve constraints
    let kindEnv = mkKindEnv assumps subst
        ps' = map (flip runReader kindEnv . enrich) ps
        p' = runReader (enrich p) kindEnv
    Impl ann ps' p' <$> solveKinds bindings

-- TODO run in KI?
inferPred :: PredKindEnv -> Pred 'Parsed -> Infer ([KAssump], [KConstraint])
inferPred predEnv p = do
  let ks = lookupPred p predEnv
      vars = getPredVars p
  results <- traverse (uncurry f) $ zip ks vars
  pure $ gatherResults results
  where
    f :: IKind -> Id -> Infer ([KAssump], [KConstraint])
    f k var = do
      kv <- IKVar <$> fresh
      let cs = [KConstraint kv k]
      pure ([(var, kv)], cs)

getPredVars :: Pred ph -> [Id]
getPredVars (IsIn _ _ tys) = mapMaybe f tys where
  f = \case
    TVar (Tyvar _ var) -> Just var
    _ -> Nothing

lookupPred :: Pred 'Parsed -> PredKindEnv -> [IKind]
lookupPred (IsIn _ predName _) predEnv =
  unsafeFromJust $ Map.lookup predName predEnv

enrichADT :: KindEnv -> ADT 'Parsed -> ADT 'KindInferred
enrichADT kindEnv adt = runReader (enrich adt) kindEnv

class Enrichable a where
  enrich :: a 'Parsed -> Reader KindEnv (a 'KindInferred)

instance Enrichable TypeAnn where
  enrich (TypeAnn sp name sch) =
    TypeAnn sp name <$> enrich sch

instance Enrichable Scheme where
  enrich (Scheme sp ps ty) =
    Scheme sp <$> traverse enrich ps <*> enrich ty

instance Enrichable Pred where
  enrich (IsIn sp name ts) =
    IsIn sp name <$> traverse enrich ts

instance Enrichable ADT where
  enrich (ADT sp hd bodies) = do
    enrichedHead <- enrich hd
    enrichedBodies <- mapM enrich bodies
    pure $ ADT sp enrichedHead enrichedBodies

instance Enrichable ADTHead where
  enrich (ADTHead name ty) = ADTHead name <$> enrich ty

instance Enrichable ConDecl where
  enrich (ConDecl sp name ty) = ConDecl sp name <$> enrich ty

instance Enrichable Type where
  enrich = \case
    TVar var -> TVar <$> enrich var
    TCon con -> TCon <$> enrich con
    TApp t1 t2 -> TApp <$> enrich t1 <*> enrich t2

instance Enrichable Tyvar where
  enrich (Tyvar sp var) = do
    k <- asks (getKindForId var)
    pure $ Tyvar (sp, k) var

instance Enrichable Tycon where
  enrich (Tycon sp con) = do
    k <- asks (getKindForId con)
    pure $ Tycon (sp, k) con

getKindForId :: Id -> Map Id IKind -> Kind
getKindForId target = normalizeKind . unsafeFromJust . Map.lookup target

graphToGroupedLists :: Ord key => [(node, key, [key])] -> [[node]]
graphToGroupedLists = map f . stronglyConnComp
  where
    f = \case
      AcyclicSCC node -> [node]
      CyclicSCC nodes -> nodes

