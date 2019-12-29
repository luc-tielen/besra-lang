
module Besra.TypeSystem.Skolems
  ( newSkolemConstant
  , newSkolemScope
  , introduceSkolemScope
  , replaceVarWithSkolem
  , replaceVarWithSkolemInExpr
  , checkForEscapedSkolems
  ) where

import Protolude hiding ( Type, TypeError )
import Control.Arrow ((***))
import Besra.Types.IR3
import Besra.Types.Ann
import Besra.Types.Id
import Besra.TypeSystem.Monad
import qualified Data.Set as Set


type KI = KindInferred
type Type' = Type KI
type Expr' = Expr KI

-- | Generate a new skolem constant
newSkolemConstant :: TypeCheckM Skolem
newSkolemConstant = do
  skolem <- gets nextSkolemVar
  modify $ \st -> st { nextSkolemVar = skolem + 1 }
  pure $ Skolem skolem

-- | Introduce skolem scope at every occurrence of a ForAll (with no scope assigned yet).
introduceSkolemScope :: Type' -> TypeCheckM Type'
introduceSkolemScope = \case
  TForAll ann name Nothing ty -> do
    scope <- Just <$> newSkolemScope
    TForAll ann name scope <$> introduceSkolemScope ty
  TForAll ann name scope ty -> do
    TForAll ann name scope <$> introduceSkolemScope ty
  TApp t1 t2 ->
    TApp <$> introduceSkolemScope t1
         <*> introduceSkolemScope t2
  other -> pure other

-- | Generate a new skolem scope
newSkolemScope :: TypeCheckM SkolemScope
newSkolemScope = do
  scope <- gets nextSkolemScope
  modify $ \st -> st { nextSkolemScope = scope + 1 }
  pure $ SkolemScope scope

-- | Skolemize a type variable by replacing all variables of the same name
--   in a type with a skolem (rigid type variable).
--   This function takes name shadowing introduced by foralls into account.
replaceVarWithSkolem :: Id -> Ann KI -> Skolem -> SkolemScope -> Type' -> Type'
replaceVarWithSkolem var ann skolem scope =
  replaceVarWithType var (TSkolem ann var scope skolem)

-- | This function skolemizes type variables with the same name appearing
--   in any type signatures in a given expression.
replaceVarWithSkolemInExpr :: Id -> Ann KI -> Skolem -> SkolemScope -> Expr' -> Expr'
replaceVarWithSkolemInExpr var ann skolem scope = go where
  go = \case
    ECon (spn, ty) con ->
      let ty' = replaceVarWithSkolem var ann skolem scope ty
       in ECon (spn, ty') con
    ELam ann' pat body ->
      ELam ann' (goPat pat) (go body)
    EApp ann' f arg -> EApp ann' (go f) (go arg)
    EIf ann' c t f -> EIf ann' (go c) (go t) (go f)
    ECase ann' e clauses ->
      ECase ann' (go e) $ map (goPat *** go) clauses
    ELet ann' bg e ->
      ELet ann' (goBg bg) (go e)
    e -> e
  goPat = \case
    PAs ann' var' pat -> PAs ann' var' $ goPat pat
    PCon (ann', ty) con pats ->
      let ty' = replaceVarWithSkolem var ann skolem scope ty
       in PCon (ann', ty') con $ map goPat pats
    p -> p
  goBg = map goExpl *** map goImpl
  goExpl (Explicit var' ty exprs) =
    let ty' = replaceVarWithSkolem var ann skolem scope ty
     in Explicit var' ty' $ map go exprs
  goImpl (Implicit var' exprs) = Implicit var' $ map go exprs

-- | Checks if any skolem variables escaped their scope and throws an error
--   if this is the case.
--
-- Every skolem variable is created when a 'ForAll' type is skolemized.
-- This determines the scope of that skolem variable, which is copied from
-- the 'SkolemScope' field of the 'ForAll' constructor.
--
-- This function traverses the tree top-down, and collects any 'SkolemScope's
-- introduced by 'ForAll's. If a 'Skolem' is encountered whose 'SkolemScope' is
-- not in the current list, then we have found an escaped skolem variable.
checkForEscapedSkolems :: Expr' -> TypeCheckM ()
checkForEscapedSkolems expr = evalStateT (go expr) Set.empty where
  go = \case
    EIf _ c t e -> go c *> go t *> go e
    ELam _ pat body -> goPat pat *> go body
    EApp _ f arg -> go f *> go arg
    ECon (_, ty) _ -> capturingState $ checkForEscapedSkolems' ty
    ECase _ e clauses -> go e *> traverse_ f clauses
      where f (pat, e') = goPat pat *> go e'
    ELet _ bg e -> capturingState $ do
      goBg bg
      go e
    _ -> pure ()
  goPat = \case
    PAs _ _ pat -> goPat pat
    PCon (_, ty) _ pats -> do
      capturingState $ checkForEscapedSkolems' ty
      traverse_ goPat pats
    _ -> pure ()
  goBg (expls, impls) = traverse_ goExpl expls *> traverse_ goImpl impls
  goExpl (Explicit _ ty exprs) = do
    checkForEscapedSkolems' ty
    traverse_ go exprs
  goImpl (Implicit _ exprs) = traverse_ go exprs

-- | On a single type signature, checks if skolems escaped their scope.
checkForEscapedSkolems' :: Type' -> StateT (Set SkolemScope) TypeCheckM ()
checkForEscapedSkolems' ty = do
  modify (collectScopes ty <>)
  scopes <- get
  let errors = [ EscapedSkolem ann name ty
               | (ann, name, scope) <- collectSkolems ty
               , scope `notElem` scopes
               ]
  case nonEmpty errors of
    Nothing -> pure ()
    Just errors' -> throwError $ MultipleErrors errors'
  where
    collectScopes = \case
      TForAll _ _ (Just scope) t -> Set.insert scope $ collectScopes t
      TForAll {} -> panic "Found unitialized skolem scope."
      _ -> mempty
    collectSkolems = \case
      TForAll _ _ _ t -> collectSkolems t
      TApp t1 t2 -> collectSkolems t1 <> collectSkolems t2
      TSkolem ann name scope _ -> [(ann, name, scope)]
      _ -> mempty

-- | Captures state before running an action and returns the state afterwards.
capturingState :: MonadState s m => m a -> m a
capturingState action = do
  s <- get
  result <- action
  modify (const s)
  pure result

