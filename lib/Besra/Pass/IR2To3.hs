
module Besra.Pass.IR2To3 ( pass ) where

import Protolude hiding ( pass )
import Unsafe ( unsafeFromJust )
import Data.Bitraversable ( bitraverse )
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Besra.Types.IR2 as IR2
import qualified Besra.Types.IR3 as IR3
import Besra.Types.CompilerState
import Besra.Types.Tyvar
import Besra.Types.Ann
import Besra.Types.Id
import Data.Graph ( SCC(..), stronglyConnComp )


{-
This pass adds more information to the AST of expressions/declarations
to prepare for typechecking:

1. Adds the type to each constructor in an expression.
2. Adds the type to each constructor in a pattern match.
3. Groups all bindings/type annotations on top level and in let expressions
   together and orders them according to what the typechecker expects.
4. Adds type annotations to all bindings in impls.
-}

type KI = KindInferred

-- | Mapping from constructor names to types.
type TypeMap = Map Id (IR3.Type KI)

newtype DesugarState = DesugarState { typeMap :: TypeMap }

type PassM = Reader DesugarState


pass :: CompilerState2 KI -> IR2.Module KI -> (IR3.Module KI, CompilerState3 KI)
pass (CompilerState2 adts traits _ kEnv) m =
  let conInfo = prepareConInfo adts
      passState = DesugarState conInfo
      m' = runReader (desugar m) passState
      traits' = map desugarTrait traits
      impls' = []  -- TODO implement for impls!
   in (m', CompilerState3 adts traits' impls' kEnv)

prepareConInfo :: [IR2.ADT KI] -> TypeMap
prepareConInfo =
  let f (IR2.ADT _ _ conDecls) = Map.fromList $ map extractType conDecls
      extractType (IR2.ConDecl ann name ty) = (name, addMissingForAlls ann ty)
   in foldMap f

class Desugar a where
  type Result a

  desugar :: a -> PassM (Result a)

instance Desugar a => Desugar [a] where
  type Result [a] = [Result a]

  desugar = traverse desugar

instance Desugar (IR2.Module KI) where
  type Result (IR2.Module KI) = IR3.Module KI

  desugar (IR2.Module decls) = do
    -- Semantic analysis only allows explicit bindings on top level.
    (expls, _) <- toBG decls
    pure $ IR3.Module expls

instance Desugar (IR2.Expr KI) where
  type Result (IR2.Expr KI) = IR3.Expr KI

  desugar = \case
    IR2.ELit ann lit -> pure $ IR3.ELit ann lit
    IR2.EVar ann var -> pure $ IR3.EVar ann var
    IR2.ECon ann name -> do
      ty <- unsafeFromJust <$> asks (Map.lookup name . typeMap)
      pure $ IR3.ECon (ann, ty) name
    IR2.ELam ann pats body -> do
      -- TODO move this transform to IR1->2
      pats' <- desugar pats
      body' <- desugar body
      let lam = IR3.ELam ann
      pure $ foldr lam body' pats'
    IR2.EApp ann f arg ->
      IR3.EApp ann <$> desugar f <*> desugar arg
    IR2.EIf ann c t f ->
      IR3.EIf ann <$> desugar c <*> desugar t <*> desugar f
    IR2.ECase ann e clauses ->
      IR3.ECase ann <$> desugar e
                    <*> traverse (bitraverse desugar desugar) clauses
    IR2.ELet ann decls expr ->
      IR3.ELet ann <$> toBG decls <*> desugar expr

instance Desugar (IR2.Pattern KI) where
  type Result (IR2.Pattern KI) = IR3.Pattern KI

  desugar = \case
    IR2.PWildcard ann -> pure $ IR3.PWildcard ann
    IR2.PLit ann lit -> pure $ IR3.PLit ann lit
    IR2.PVar ann var -> pure $ IR3.PVar ann var
    IR2.PCon ann name pats -> do
      ty <- unsafeFromJust <$> asks (Map.lookup name . typeMap)
      IR3.PCon (ann, ty) name <$> desugar pats
    IR2.PAs ann name pat -> IR3.PAs ann name <$> desugar pat

desugarPred :: IR2.Pred KI -> IR3.Pred KI
desugarPred (IR2.IsIn ann name tys) =
  IR3.IsIn ann name $ map desugarType tys

desugarTrait :: IR2.Trait KI -> IR3.Trait KI
desugarTrait (IR2.Trait ann ps p ts) =
  let ps' = map desugarPred ps
      p' = desugarPred p
      ts' = Map.fromList $ map desugarTypeAnn ts
   in IR3.Trait ann ps' p' ts'

desugarTypeAnn :: IR2.TypeAnn KI -> (Id, IR3.Type KI)
desugarTypeAnn (IR2.TypeAnn _ name (IR2.Scheme ann ps ty)) =
  case ps of
    [] -> (name, addMissingForAlls ann ty)
    _ -> typeclassesNotImplementedError

desugarType :: IR2.Type ph -> IR3.Type ph
desugarType = \case
  IR2.TCon tycon -> IR3.TCon tycon
  IR2.TVar tyvar -> IR3.TVar tyvar
  IR2.TApp t1 t2 -> IR3.TApp (desugarType t1) (desugarType t2)

toBG :: [IR2.Decl KI] -> PassM (IR3.BindGroup KI)
toBG decls = do
  let groupedDecls = groupDecls decls
      (expDecls, impDecls) = List.partition hasTypeAnn groupedDecls
      hasTypeAnn (_, (ta, _)) = isJust ta
  exps <- traverse toExplicit expDecls
  imps <- toImplicits impDecls
  pure (exps, imps)

groupDecls :: [IR2.Decl KI] -> [(Id, (Maybe (IR2.Scheme KI), [IR2.Binding KI]))]
groupDecls decls = Map.toList $ foldr' f Map.empty decls
  where
    f = \case
      IR2.TypeAnnDecl ta -> Map.alter (addScheme ta) (typeAnnName ta)
      IR2.BindingDecl b -> Map.alter (addBinding b) (bindingName b)
    addBinding b = \case
      Nothing -> Just (Nothing, [b])
      Just (ta, bs) -> Just (ta, b:bs)
    addScheme (IR2.TypeAnn _ _ sch) = \case
      Nothing -> Just (Just sch, [])
      Just (_, bs) -> Just (Just sch, bs)

toExplicit :: (Id, (Maybe (IR2.Scheme KI), [IR2.Binding KI]))
           -> PassM (IR3.Explicit KI)
toExplicit (name, (Just (IR2.Scheme ann ps ty), bs)) = do
  when (ps /= []) typeclassesNotImplementedError
  exprs <- traverse convertBinding bs
  pure $ IR3.Explicit name (addMissingForAlls ann ty) exprs
toExplicit (Id name, (Nothing, _)) =
  panic $ "Error in 'toExplicit' in IR2->3 pass for id = " <> name

convertBinding :: IR2.Binding KI -> PassM (IR3.Expr KI)
convertBinding (IR2.Binding _ _ e) = desugar e

toImplicits :: [(Id, (Maybe a, [IR2.Binding KI]))]
            -> PassM [IR3.Implicit KI]
toImplicits decls = sortDecls <$> traverse f decls where
  sortDecls = concatMap g . stronglyConnComp
  toImplicit name bs = IR3.Implicit name <$> traverse convertBinding bs
  names = map fst decls
  f (name, (_, bs)) = do
    implicit <- toImplicit name bs
    let referredNames = foldMap refersTo bs
    pure (implicit, name, names `List.intersect` referredNames)
  g = \case
    AcyclicSCC node -> [node]
    CyclicSCC nodes -> nodes

class RefersTo a where
  refersTo :: a -> [Id]

instance RefersTo a => RefersTo [a] where
  refersTo = foldMap refersTo

instance RefersTo b => RefersTo (a, b) where
  refersTo (_, b) = refersTo b

instance RefersTo (IR2.Binding ph) where
  refersTo (IR2.Binding _ _ expr) = refersTo expr

instance RefersTo (IR2.Expr ph) where
  refersTo = \case
    IR2.ELit {} -> mempty
    IR2.ECon {} -> mempty
    IR2.EVar _ name -> [name]
    IR2.ELam _ _ body -> refersTo body
    IR2.EApp _ f arg -> refersTo f <> refersTo arg
    IR2.EIf _ c t f -> refersTo c <> refersTo t <> refersTo f
    IR2.ECase _ expr clauses -> refersTo expr <> refersTo clauses
    IR2.ELet _ decls body -> refersTo decls <> refersTo body

instance RefersTo (IR2.Decl ph) where
  refersTo = \case
    IR2.BindingDecl b -> refersTo b
    IR2.TypeAnnDecl _ -> mempty

bindingName :: IR2.Binding KI -> Id
bindingName (IR2.Binding _ name _) = name

typeAnnName :: IR2.TypeAnn KI -> Id
typeAnnName (IR2.TypeAnn _ name _) = name

typeclassesNotImplementedError :: a
typeclassesNotImplementedError =
  panic "Typeclasses not implemented yet in IR3 / typesystem."

-- TODO move this to an earlier pass once scheme has been refactored there
addMissingForAlls :: IR3.Ann ph -> IR2.Type ph -> IR3.Type ph
addMissingForAlls ann ty =
  let vars = ftv ty
      ty' = desugarType ty
   in foldr (\var -> IR3.TForAll ann var Nothing) ty' vars

ftv :: IR2.Type ph -> Set Id
ftv = \case
  IR2.TCon _ -> mempty
  IR2.TVar (Tyvar _ var) -> [var]
  IR2.TApp t1 t2 -> ftv t1 <> ftv t2

