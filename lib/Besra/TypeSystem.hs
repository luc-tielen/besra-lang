
module Besra.TypeSystem ( checkModule ) where

import Protolude hiding ( Type, TypeError, check )
import qualified Data.Map as Map
import Besra.TypeSystem.Monad
import Besra.TypeSystem.Skolems
import Besra.TypeSystem.Unification
import Besra.Types.IR3
import Besra.Types.Kind
import Besra.Types.Span
import Besra.Types.Ann
import Besra.Types.Id


type KI = KindInferred
type Type' = Type KI

checkModule :: Module KI -> TypeCheckM (Module TC)
checkModule (Module expls) = do
  -- TODO make parallel
  expls' <- traverse (checkExpl bindings) expls
  pure $ Module expls'
  where bindings = Map.fromList [(name, ty) | Explicit name ty _ <- expls]

check :: Type' -> Expr KI -> TypeCheckM (Expr TC)
check t e = addContext (WhileChecking t e) (check' t e)

check' :: Type' -> Expr KI -> TypeCheckM (Expr TC)
check' (TForAll ann var _ ty) expr = do
  scope <- newSkolemScope
  skolem <- newSkolemConstant
  let ty' = replaceVarWithSkolem var ann skolem scope ty
      expr' = replaceVarWithSkolemInExpr var ann skolem scope expr
  typedExpr <- check ty' expr'
  pure $ overrideType (TForAll ann var (Just scope) ty) typedExpr
check' unknown@(TUnknown _) e = do
  e' <- infer e
  let ty = typeOf e'
  ty' <- instantiatePolyTypeWithUnknowns ty
  unify ty' unknown
  pure $ overrideType ty' e'
check' expectedType e@(ELam ann pat body) = do
  case expectedType of
    TArrow _ t1 t2 -> do
      (pat', bindings) <- checkPat t1 pat
      bindLocalVariables bindings $ do
        body' <- check t2 body
        pure $ ELam (addType ann expectedType) pat' body'
    _ -> throwError $ ExpectedArrowType e expectedType
check' expectedType (EIf ann c t f) = do
  c' <- check (tBool ann) c
  t' <- check expectedType t
  f' <- check expectedType f
  pure $ EIf (addType ann expectedType) c' t' f'
check' expectedType (ECase ann e clauses) = do
  e' <- infer e
  patType <- instantiatePolyTypeWithUnknowns (typeOf e')
  clauses' <- traverse (uncurry $ checkClause expectedType patType) clauses
  pure $ ECase (addType ann expectedType) e' clauses'
check' expectedType (EVar ann var) = do
  varType <- introduceSkolemScope <=< lookupVariable $ var
  expectedType' <- introduceSkolemScope expectedType
  unify expectedType' varType
  pure $ EVar (addType ann expectedType') var
check' expectedType (ECon (ann, ty) con) = do
  expectedType' <- introduceSkolemScope expectedType
  ty' <- introduceSkolemScope ty  -- TODO instantiatePolyTypeWithUnknowns?
  unify expectedType' ty'
  pure $ ECon (addType ann expectedType') con
check' expectedType (EApp ann f arg) = do
  f' <- infer f
  let fnType = typeOf f'
  app <- checkFunctionApplication ann f' fnType arg
  unify expectedType (typeOf app)
  pure app
check' expectedType (ELet ann bg e) = do
  (bg', e') <- inferLetExpr bg e (check expectedType)
  pure $ ELet (addType ann expectedType) bg' e'
check' expectedType e = do
  typedExpr <- infer e
  let actualType = typeOf typedExpr
  unify expectedType actualType
  pure typedExpr

infer :: Expr KI -> TypeCheckM (Expr TC)
infer e = addContext (WhileInferring e) (infer' e)

infer' :: Expr KI -> TypeCheckM (Expr TC)
infer' = \case
  ELit ann lit -> do
    let ty = inferLitType ann lit
    pure $ ELit (addType ann ty) lit
  EVar ann var -> do
    ty <- introduceSkolemScope <=< lookupVariable $ var
    pure $ EVar (addType ann ty) var
  ECon (ann, ty) con -> do
    ty' <- introduceSkolemScope ty
    pure $ ECon (addType ann ty') con
  ELam ann pat body -> do
    ty <- freshUnificationVar
    (pat', bindings) <- checkPat ty pat
    bindLocalVariables bindings $ do
      body' <- infer body
      let bodyType = typeOf body'
      bodyType' <- instantiatePolyTypeWithUnknowns bodyType
      let lamType = TArrow (span ann, fnKind) ty bodyType'
      pure $ ELam (addType ann lamType) pat' body'
  EApp ann f arg -> do
    f' <- infer f
    let fType = typeOf f'
    checkFunctionApplication ann f' fType arg
  EIf ann c t e -> do
    c' <- check (tBool ann) c
    t' <- infer t
    e' <- infer e
    let tyT = typeOf t'
        tyE = typeOf e'
    tyT' <- instantiatePolyTypeWithUnknowns tyT
    tyE' <- instantiatePolyTypeWithUnknowns tyE
    unify tyT' tyE'
    pure $ EIf (addType ann tyT') c' t' e'
  ECase ann e clauses -> do
    e' <- infer e
    patType <- instantiatePolyTypeWithUnknowns (typeOf e')
    ty <- freshUnificationVar
    clauses' <- traverse (uncurry $ checkClause ty patType) clauses
    pure $ ECase (addType ann ty) e' clauses'
  ELet ann bg e -> do
    (bg', e') <- inferLetExpr bg e infer
    let ty = typeOf e'
    pure $ ELet (addType ann ty) bg' e'

inferLetExpr :: BindGroup KI -> Expr KI
         -> (Expr KI -> TypeCheckM (Expr TC))
         -> TypeCheckM (BindGroup TC, Expr TC)
inferLetExpr (expls, impls) expr handleExpr = do
  let explBindings = [(name, ty) | Explicit name ty _ <- expls]
  (implBindings, impls') <- unzip <$> traverse implToExpl impls
  let bindings = Map.fromList $ explBindings <> implBindings
  expls' <- traverse (checkExpl bindings) expls
  impls'' <- traverse (inferImpl bindings) impls'
  expr' <- bindLocalVariables bindings $ handleExpr expr
  pure ((expls' <> impls'', []), expr')
  where
    implToExpl (Implicit name exprs) = do
      ty <- freshUnificationVar
      pure ((name, ty), Explicit name ty exprs)

checkExpl :: TypeEnv -> Explicit KI -> TypeCheckM (Explicit TC)
checkExpl bindings (Explicit name ty exprs) = do
  ty' <- introduceSkolemScope <=< instantiatePolyTypeWithUnknowns $ ty
  exprs' <- bindLocalVariables bindings $ traverse (check ty') exprs
  pure $ Explicit name ty' exprs'

-- NOTE: The Implicit has already been converted to an Explicit here
-- (containing a unification variable as type).
inferImpl :: TypeEnv -> Explicit KI -> TypeCheckM (Explicit TC)
inferImpl bindings (Explicit name ty exprs) = bindLocalVariables bindings $ do
  exprs' <- for exprs $ \e -> do
    e' <- infer e
    let ty' = typeOf e'  -- TODO inst poly type?
    unify ty ty'
    pure e'
  pure $ Explicit name ty exprs'

inferLitType :: Ann KI -> Lit -> Type KI
inferLitType ann = \case
  LChar _ -> tChar ann
  LString _ -> tString ann
  LNumber _ -> tNumber ann

checkClause :: Type KI -> Type KI -> Pattern KI -> Expr KI
            -> TypeCheckM (Pattern TC, Expr TC)
checkClause expectedType patTy pat expr = do
  (pat', bindings) <- checkPat patTy pat
  bindLocalVariables bindings $ do
    expr' <- check expectedType expr
    pure (pat', expr')

checkPat :: Type KI -> Pattern KI -> TypeCheckM (Pattern TC, TypeEnv)
checkPat expectedType = \case
  PWildcard ann ->
    pure (PWildcard (addType ann expectedType), mempty)
  PVar ann var ->
    pure (PVar (addType ann expectedType) var, Map.singleton var expectedType)
  PLit ann lit -> do
    let ty = inferLitType ann lit
    unify expectedType ty
    pure (PLit (addType ann expectedType) lit, mempty)
  PAs ann name pat -> do
    (pat', env) <- checkPat expectedType pat
    pure (PAs (addType ann expectedType) name pat', Map.insert name expectedType env)
  PCon (spn, ty) name pats -> do
    ty' <- introduceSkolemScope <=< instantiatePolyTypeWithUnknowns $ ty
    expectedType' <- introduceSkolemScope expectedType
    let (argTys, retTy) = deconstructType ty'
        expectedArity = length argTys
        actualArity = length pats
    when (expectedArity /= actualArity) $
      throwError $ PatternArityMismatch name ty expectedArity actualArity
    (pats', envs) <- unzip <$> zipWithM checkPat argTys pats
    unify expectedType' retTy
    pure (PCon (addType spn expectedType') name pats', Map.unions envs)

-- | Helper function that splits a type into the types of the arguments
--   and it's return type.
deconstructType :: Type ph -> ([Type ph], Type ph)
deconstructType t =
  let (argsFn, ret) = go identity t
    in (argsFn [], ret)
  where
    go args (TArrow _ t1 t2) = go (args . (t1 :)) t2
    go args ty = (args, ty)

-- | Remove any foralls in a type by introducing new unification variables.
--
-- This is necessary during type checking to avoid unifying a polymorphic type with a
-- unification variable.
instantiatePolyTypeWithUnknowns
  :: Type' -> TypeCheckM Type'
instantiatePolyTypeWithUnknowns (TForAll _ name _ ty) = do
  ty' <- replaceVarWithUnknown name ty
  instantiatePolyTypeWithUnknowns ty'
instantiatePolyTypeWithUnknowns ty = pure ty

-- | Replaces all occurrences of a var with a fresh unification variable.
replaceVarWithUnknown :: Id -> Type' -> TypeCheckM Type'
replaceVarWithUnknown var ty = do
  unknown <- freshUnificationVar
  pure $ replaceVarWithType var unknown ty

-- | Check the type of a function application
checkFunctionApplication :: Ann KI -> Expr TC -> Type KI -> Expr KI -> TypeCheckM (Expr TC)
checkFunctionApplication ann fn fnTy arg = do
  subst <- gets substitution
  checkFunctionApplication' ann fn (substituteType subst fnTy) arg

checkFunctionApplication' :: Ann KI -> Expr TC -> Type KI -> Expr KI -> TypeCheckM (Expr TC)
checkFunctionApplication' ann fn (TArrow _ argTy retTy) arg = do
  arg' <- check argTy arg
  pure $ EApp (addType ann retTy) fn arg'
checkFunctionApplication' ann fn (TForAll _ var _ ty) arg = do
  ty' <- replaceVarWithUnknown var ty
  checkFunctionApplication ann fn ty' arg
checkFunctionApplication' ann fn ty arg = do
  argument <- do
    arg' <- infer arg
    let ty' = typeOf arg'
    ty'' <- instantiatePolyTypeWithUnknowns ty'
    pure $ overrideType ty'' arg'
  retTy <- freshUnificationVar
  let argType = typeOf argument
  unify ty (TArrow (span ty, fnKind) argType retTy)
  pure $ EApp (addType ann retTy) fn argument

class HasType a where
  typeOf :: a -> Type KI

instance HasType (Expr TC) where
  typeOf = \case
    ELit ann _ -> typeOf ann
    EVar ann _ -> typeOf ann
    ECon ann _ -> typeOf ann
    ELam ann _ _ -> typeOf ann
    EApp ann _ _ -> typeOf ann
    EIf ann _ _ _ -> typeOf ann
    ECase ann _ _ -> typeOf ann
    ELet ann _ _ -> typeOf ann

instance HasType b => HasType (a, b) where
  typeOf (_, b) = typeOf b

instance HasType (Type KI) where
  typeOf = identity

addType :: Ann KI -> Type KI -> Ann TC
addType ann ty = (span ann, ty)

overrideType :: Type KI -> Expr TC -> Expr TC
overrideType ty = \case
  ELit ann lit -> ELit (override ann) lit
  EVar ann var -> EVar (override ann) var
  ECon ann con -> ECon (override ann) con
  ELam ann pat body -> ELam (override ann) pat body
  EApp ann f arg -> EApp (override ann) f arg
  EIf ann c t f -> EIf (override ann) c t f
  ECase ann e clauses -> ECase (override ann) e clauses
  ELet ann bg e -> ELet (override ann) bg e
  where override ann = (span ann, ty)

mkCon :: Id -> Ann KI -> Type KI
mkCon c ann = TCon (Tycon (ann, Star) c)

tBool, tChar, tString, tNumber :: Ann KI -> Type KI
tBool = mkCon (Id "Bool")
tChar = mkCon (Id "Char")
tString = mkCon (Id "String")
tNumber = mkCon (Id "Number")

fnKind :: Kind
fnKind = KArr Star (KArr Star Star)

