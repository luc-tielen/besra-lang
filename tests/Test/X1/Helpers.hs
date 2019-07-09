
module Test.X1.Helpers ( module Test.X1.Helpers ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Module
import X1.Types.Expr1.Trait
import X1.Types.Expr1.Impl
import X1.Types.Expr1.TypeAnn
import X1.Types.Expr1.Expr
import X1.Types.Expr1.ADT
import X1.Types.Expr1.Scheme
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Type
import X1.Types.Ann


emptyAnn :: ()
emptyAnn = ()

class StripAnns a where
  type Result a

  stripAnns :: a -> Result a

instance StripAnns (Module ph) where
  type Result (Module ph) = Module 'Testing

  stripAnns (Module decls) =
    Module (stripAnns decls)

instance StripAnns (Decl ph) where
  type Result (Decl ph) = Decl 'Testing

  stripAnns = \case
    TypeAnnDecl typeAnn -> TypeAnnDecl (stripAnns typeAnn)
    DataDecl adt -> DataDecl (stripAnns adt)
    TraitDecl trait -> TraitDecl (stripAnns trait)
    ImplDecl _ impl -> ImplDecl emptyAnn (stripAnns impl)
    BindingDecl binding -> BindingDecl (stripAnns binding)
    FixityDecl _ fixity prio op -> FixityDecl emptyAnn fixity prio op

instance StripAnns (ADT ph) where
  type Result (ADT ph) = ADT 'Testing

  stripAnns (ADT _ adtHead body) =
    ADT emptyAnn (stripAnns adtHead) (stripAnns body)

instance StripAnns (ADTHead ph) where
  type Result (ADTHead ph) = ADTHead 'Testing

  stripAnns (ADTHead con vars) =
    ADTHead (stripAnns con) (stripAnns vars)

instance StripAnns (ConDecl ph) where
  type Result (ConDecl ph) = ConDecl 'Testing

  stripAnns (ConDecl name tys) =
    ConDecl name (stripAnns tys)

instance StripAnns a => StripAnns [a] where
  type Result [a] = [Result a]

  stripAnns = map stripAnns

instance StripAnns b => StripAnns (a, b) where
  type Result (a, b) = (a, Result b)

  stripAnns = map stripAnns

instance StripAnns (Trait ph) where
  type Result (Trait ph) = Trait 'Testing

  stripAnns (Trait ps p tys) =
    Trait (stripAnns ps) (stripAnns p) (stripAnns tys)

instance StripAnns (Impl ph) where
  type Result (Impl ph) = Impl 'Testing

  stripAnns (Impl _ ps p bindings) =
    Impl emptyAnn (stripAnns ps) (stripAnns p) (stripAnns bindings)

instance StripAnns (Pred ph) where
  type Result (Pred ph) = Pred 'Testing

  stripAnns (IsIn name tys) = IsIn name (stripAnns tys)

instance StripAnns (Binding ph) where
  type Result (Binding ph) = Binding 'Testing

  stripAnns (Binding _ name expr) =
    Binding emptyAnn name (stripAnns expr)

instance StripAnns (Expr1 ph) where
  type Result (Expr1 ph) = Expr1 'Testing

  stripAnns = \case
    E1Lit _ lit -> E1Lit emptyAnn lit
    E1Var _ var -> E1Var emptyAnn var
    E1Con _ con -> E1Con emptyAnn con
    E1Lam _ pats body -> E1Lam emptyAnn pats (stripAnns body)
    E1App _ f args -> E1App emptyAnn (stripAnns f) (stripAnns args)
    E1BinOp _ op l r -> E1BinOp emptyAnn (stripAnns op) (stripAnns l) (stripAnns r)
    E1Neg _ e -> E1Neg emptyAnn (stripAnns e)
    E1If _ c tr fl -> E1If emptyAnn (stripAnns c) (stripAnns tr) (stripAnns fl)
    E1Case _ e clauses -> E1Case emptyAnn (stripAnns e) (stripAnns clauses)
    E1Let _ decls body -> E1Let emptyAnn (stripAnns decls) (stripAnns body)
    E1Parens _ e -> E1Parens emptyAnn (stripAnns e)

instance StripAnns (ExprDecl ph) where
  type Result (ExprDecl ph) = ExprDecl 'Testing

  stripAnns = \case
    ExprTypeAnnDecl typeAnn ->
      ExprTypeAnnDecl (stripAnns typeAnn)
    ExprBindingDecl binding ->
      ExprBindingDecl (stripAnns binding)
    ExprFixityDecl _ fixity prio op ->
      ExprFixityDecl emptyAnn fixity prio op

instance StripAnns (TypeAnn ph) where
  type Result (TypeAnn ph) = TypeAnn 'Testing

  stripAnns (TypeAnn name scheme) =
    TypeAnn name (stripAnns scheme)

instance StripAnns (Scheme ph) where
  type Result (Scheme ph) = Scheme 'Testing

  stripAnns (Scheme ps ty) =
    Scheme (stripAnns ps) (stripAnns ty)

instance StripAnns (Type ph) where
  type Result (Type ph) = Type 'Testing

  stripAnns = \case
    TCon tycon -> TCon (stripAnns tycon)
    TVar tycon -> TVar (stripAnns tycon)
    TApp t1 ts -> TApp (stripAnns t1) (stripAnns ts)

instance StripAnns (Tycon ph) where
  type Result (Tycon ph) = Tycon 'Testing

  stripAnns (Tycon _ con) = Tycon emptyAnn con

instance StripAnns (Tyvar ph) where
  type Result (Tyvar ph) = Tyvar 'Testing

  stripAnns (Tyvar _ var) = Tyvar emptyAnn var

{-
type Transform =
  Handlers Identity 'Parsed
    (Module 'Testing) (Decl 'Testing) (Impl 'Testing)
    (Binding 'Testing) (ExprDecl 'Testing) (Expr1 'Testing)

type Result a ph =
  FoldResult a (Module ph) (Decl ph) (Impl ph)
               (Binding ph) (ExprDecl ph) (Expr1 ph)

stripAnns :: Fold a => a 'Parsed -> Result a 'Testing
stripAnns ast =
  let fs :: Transform
      fs = Handlers
        { handlersM = fsM
        , handlersD = fsD
        , handlersI = fsI
        , handlersB = fsB
        , handlersED = fsED
        , handlersE = fsE
        }
      fsM = HandlersM { moduleM = pure . Module }
      fsD = HandlersD
        { typeAnnD = pure . TypeAnnDecl
        , adtD = pure . DataDecl . stripAnnAdt
        , traitD = pure . TraitDecl
        , implD = \_ impl -> pure $ ImplDecl emptyAnn impl
        , bindingD = pure . BindingDecl
        , fixityD = \_ fx prec name -> pure $ FixityDecl emptyAnn fx prec name
        }
      fsI = HandlersI { implI = \_ ps p bs -> pure $ Impl emptyAnn ps p bs }
      fsB = HandlersB { bindingB = \_ name expr -> pure $ Binding emptyAnn name expr }
      fsED = HandlersED
        { typeAnnED = pure . ExprTypeAnnDecl
        , bindingED = pure . ExprBindingDecl
        , fixityED = \_ fx prec name -> pure $ ExprFixityDecl emptyAnn fx prec name
        }
      fsE = HandlersE
        { litE = \_ lit -> pure $ E1Lit emptyAnn lit
        , varE = \_ var -> pure $ E1Var emptyAnn var
        , conE = \_ con -> pure $ E1Con emptyAnn con
        , lamE = \_ pats body -> pure $ E1Lam emptyAnn pats body
        , appE = \_ func args -> pure $ E1App emptyAnn func args
        , binOpE = \_ op l r -> pure $ E1BinOp emptyAnn op l r
        , negE = \_ e -> pure $ E1Neg emptyAnn e
        , ifE = \_ c tr fl -> pure $ E1If emptyAnn c tr fl
        , caseE = \_ e cs -> pure $ E1Case emptyAnn e cs
        , letE = \_ decls body -> pure $ E1Let emptyAnn decls body
        , parenE = \_ e -> pure $ E1Parens emptyAnn e
        }
   in runIdentity $ foldAST fs ast

stripAnnAdt :: ADT 'Parsed -> ADT 'Testing
stripAnnAdt (ADT _ adtHead adtBody) = ADT emptyAnn adtHead adtBody

emptyAnn :: Ann 'Testing
emptyAnn = ()
-}

