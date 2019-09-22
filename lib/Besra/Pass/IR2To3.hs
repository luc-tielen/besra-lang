
module Besra.Pass.IR2To3 ( pass ) where

import Protolude hiding ( pass )
import Unsafe ( unsafeFromJust )
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Besra.Types.IR2 as IR2
import qualified Besra.Types.IR3 as IR3
import Besra.Types.IR3 ( Qual(..) )
import Besra.Types.CompilerState
import Besra.Types.Ann
import Besra.Types.Id
import Data.Graph ( SCC(..), stronglyConnComp )


{-
This pass adds more information to the AST of expressions/declarations
to prepare for typechecking:

1. Adds the scheme to each constructor in an expression.
2. Adds the scheme to each constructor in a pattern match.
3. Groups all bindings/type annotations on top level and in let expressions
   together and orders them according to what the typechecker expects.
-}

type KI = KindInferred

-- | Mapping from constructor names to type schemes.
type SchemeMap = Map Id (IR3.Scheme KI)

type PassM = Reader SchemeMap

pass :: CompilerState KI -> IR2.Module KI -> IR3.Module KI
pass (CompilerState adts _ _ _) m =
  let conInfo = prepareConInfo adts
   in runReader (desugar m) conInfo

prepareConInfo :: [IR2.ADT KI] -> SchemeMap
prepareConInfo =
  let f (IR2.ADT _ _ conDecls) = Map.fromList $ map extractScheme conDecls
      extractScheme (IR2.ConDecl ann name ty) = (name, toScheme ann ty)
   in foldMap f

-- TODO rename
toScheme :: Ann KI -> IR2.Type KI -> IR3.Scheme KI
toScheme ann ty = IR3.Forall ann [] ([] :=> toType ty)

-- TODO rename
toType :: IR2.Type KI -> IR3.Type KI
toType = \case
  IR2.TCon tycon -> IR3.TCon tycon
  IR2.TVar tyvar -> IR3.TVar tyvar
  IR2.TApp t1 t2 -> IR3.TApp (toType t1) (toType t2)

-- TODO rename
toPred :: IR2.Pred KI -> IR3.Pred KI
toPred (IR2.IsIn ann name tys) =
  IR3.IsIn ann name $ map toType tys

-- TODO remove typeclass?
class Desugar a where
  type Result a

  desugar :: a -> PassM (Result a)

instance Desugar a => Desugar [a] where
  type Result [a] = [Result a]

  desugar = traverse desugar

instance Desugar (IR2.Module KI) where
  type Result (IR2.Module KI) = IR3.Module KI

  desugar (IR2.Module decls) = do
    IR3.Module <$> toBG decls

instance Desugar (IR2.Expr KI) where
  type Result (IR2.Expr KI) = IR3.Expr KI

  desugar = \case
    IR2.ELit ann lit -> pure $ IR3.ELit ann lit
    IR2.EVar ann var -> pure $ IR3.EVar ann var
    IR2.ECon ann name -> do
      sch <- unsafeFromJust <$> asks (Map.lookup name)
      pure $ IR3.ECon ann name sch
    IR2.ELam ann pats body ->
      -- NOTE: this only applies to anonymous lambdas
      IR3.ELam ann <$> desugar pats <*> desugar body
    IR2.EApp ann f arg ->
      IR3.EApp ann <$> desugar f <*> desugar arg
    IR2.EIf ann c t f ->
      IR3.EIf ann <$> desugar c <*> desugar t <*> desugar f
    IR2.ECase ann e clauses ->
      IR3.ECase ann <$> desugar e <*> traverse f clauses
      where f (pat, expr) = (,) <$> desugar pat <*> desugar expr
    IR2.ELet ann decls expr ->
      IR3.ELet ann <$> toBG decls <*> desugar expr

instance Desugar IR2.Pattern where
  type Result IR2.Pattern = IR3.Pattern KI

  desugar = \case
    IR2.PWildcard -> pure IR3.PWildcard
    IR2.PLit lit -> pure $ IR3.PLit lit
    IR2.PVar var -> pure $ IR3.PVar var
    IR2.PCon name pats -> do
      sch <- unsafeFromJust <$> asks (Map.lookup name)
      IR3.PCon name sch <$> desugar pats
    IR2.PAs name pat -> IR3.PAs name <$> desugar pat


toBG :: [IR2.Decl KI] -> PassM (IR3.BindGroup KI)
toBG decls = do
  let groupedDecls = groupDecls decls
      (expDecls, impDecls) = List.partition hasTypeAnn groupedDecls
      hasTypeAnn (_, (ta, _)) = isJust ta
  exps <- traverse toExplicit expDecls
  imps <- toImplicits impDecls
  pure (exps, imps)

groupDecls :: [IR2.Decl KI]
           -> [(Id, (Maybe (IR2.Scheme KI), [IR2.Binding KI]))]
groupDecls decls = map reverseBindings $ Map.toList results
  where
    results = foldl' (flip f) Map.empty decls
    f = \case
      IR2.TypeAnnDecl ta -> Map.alter (addScheme ta) (typeAnnName ta)
      IR2.BindingDecl b -> Map.alter (addBinding b) (bindingName b)
    addBinding b = \case
      Nothing -> Just (Nothing, [b])
      Just (ta, bs) -> Just (ta, b:bs)
    addScheme (IR2.TypeAnn _ _ sch) = \case
      Nothing -> Just (Just sch, [])
      Just (_, bs) -> Just (Just sch, bs)
    reverseBindings (name, (ta, bs)) = (name, (ta, reverse bs))

toExplicit :: (Id, (Maybe (IR2.Scheme KI), [IR2.Binding KI]))
           -> PassM (IR3.Explicit KI)
toExplicit (name, (Just (IR2.Scheme ann ps ty), bs)) = do
  bs' <- traverse convertBinding bs
  pure (name, IR3.Forall ann [] (map toPred ps :=> toType ty), bs')
toExplicit (Id name, (Nothing, _)) =
  panic $ "Error in 'toExplicit' in IR2->3 pass for id = " <> name

convertBinding :: IR2.Binding KI -> PassM (IR3.Alt KI)
convertBinding (IR2.Binding _ _ e) = case e of
  IR2.ELam _ pats body -> (,) <$> desugar pats <*> desugar body
  _ -> ([], ) <$> desugar e


toImplicits :: [(Id, (Maybe (IR2.Scheme KI), [IR2.Binding KI]))]
            -> PassM [[IR3.Implicit KI]]
toImplicits decls = sortDecls <$> traverse f decls where
  names = map fst decls
  f (name, (_, bs)) = do
    implicit <- toImplicit name bs
    let referredNames = foldMap refersTo bs
    pure (implicit, name, names `List.intersect` referredNames)
  toImplicit name bs = (name,) <$> traverse convertBinding bs
  sortDecls = map g . stronglyConnComp
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

