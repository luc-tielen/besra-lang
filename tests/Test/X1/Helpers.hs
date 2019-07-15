
module Test.X1.Helpers ( module Test.X1.Helpers ) where

import Protolude hiding ( Type )
import X1.Types.IR1.Module
import X1.Types.IR1.Trait
import X1.Types.IR1.Impl
import X1.Types.IR1.TypeAnn
import X1.Types.IR1.Expr
import X1.Types.IR1.ADT
import X1.Types.IR1.Scheme
import X1.Types.IR1.Pred
import X1.Types.IR1.Type
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
    ImplDecl impl -> ImplDecl (stripAnns impl)
    BindingDecl binding -> BindingDecl (stripAnns binding)
    FixityDecl fixity -> FixityDecl (stripAnns fixity)

instance StripAnns (FixityInfo ph) where
  type Result (FixityInfo ph) = FixityInfo 'Testing

  stripAnns (FixityInfo _ fixity prio name) =
    FixityInfo emptyAnn fixity prio name

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

  stripAnns (ConDecl _ name tys) =
    ConDecl emptyAnn name (stripAnns tys)

instance StripAnns a => StripAnns [a] where
  type Result [a] = [Result a]

  stripAnns = map stripAnns

instance StripAnns b => StripAnns (a, b) where
  type Result (a, b) = (a, Result b)

  stripAnns = map stripAnns

instance StripAnns (Trait ph) where
  type Result (Trait ph) = Trait 'Testing

  stripAnns (Trait _ ps p tys) =
    Trait emptyAnn (stripAnns ps) (stripAnns p) (stripAnns tys)

instance StripAnns (Impl ph) where
  type Result (Impl ph) = Impl 'Testing

  stripAnns (Impl _ ps p bindings) =
    Impl emptyAnn (stripAnns ps) (stripAnns p) (stripAnns bindings)

instance StripAnns (Pred ph) where
  type Result (Pred ph) = Pred 'Testing

  stripAnns (IsIn _ name tys) =
    IsIn emptyAnn name (stripAnns tys)

instance StripAnns (Binding ph) where
  type Result (Binding ph) = Binding 'Testing

  stripAnns (Binding _ name expr) =
    Binding emptyAnn name (stripAnns expr)

instance StripAnns (Expr ph) where
  type Result (Expr ph) = Expr 'Testing

  stripAnns = \case
    ELit _ lit -> ELit emptyAnn lit
    EVar _ var -> EVar emptyAnn var
    ECon _ con -> ECon emptyAnn con
    ELam _ pats body -> ELam emptyAnn pats (stripAnns body)
    EApp _ f args -> EApp emptyAnn (stripAnns f) (stripAnns args)
    EBinOp _ op l r -> EBinOp emptyAnn (stripAnns op) (stripAnns l) (stripAnns r)
    ENeg _ e -> ENeg emptyAnn (stripAnns e)
    EIf _ c tr fl -> EIf emptyAnn (stripAnns c) (stripAnns tr) (stripAnns fl)
    ECase _ e clauses -> ECase emptyAnn (stripAnns e) (stripAnns clauses)
    ELet _ decls body -> ELet emptyAnn (stripAnns decls) (stripAnns body)
    EParens _ e -> EParens emptyAnn (stripAnns e)

instance StripAnns (ExprDecl ph) where
  type Result (ExprDecl ph) = ExprDecl 'Testing

  stripAnns = \case
    ExprTypeAnnDecl typeAnn ->
      ExprTypeAnnDecl (stripAnns typeAnn)
    ExprBindingDecl binding ->
      ExprBindingDecl (stripAnns binding)
    ExprFixityDecl fixity ->
      ExprFixityDecl (stripAnns fixity)

instance StripAnns (TypeAnn ph) where
  type Result (TypeAnn ph) = TypeAnn 'Testing

  stripAnns (TypeAnn _ name scheme) =
    TypeAnn emptyAnn name (stripAnns scheme)

instance StripAnns (Scheme ph) where
  type Result (Scheme ph) = Scheme 'Testing

  stripAnns (Scheme _ ps ty) =
    Scheme emptyAnn (stripAnns ps) (stripAnns ty)

instance StripAnns (Type ph) where
  type Result (Type ph) = Type 'Testing

  stripAnns = \case
    TCon tycon -> TCon (stripAnns tycon)
    TVar tycon -> TVar (stripAnns tycon)
    TApp t1 ts -> TApp (stripAnns t1) (stripAnns ts)
    TParen _ t -> TParen emptyAnn (stripAnns t)

instance StripAnns (Tycon ph) where
  type Result (Tycon ph) = Tycon 'Testing

  stripAnns (Tycon _ con) = Tycon emptyAnn con

instance StripAnns (Tyvar ph) where
  type Result (Tyvar ph) = Tyvar 'Testing

  stripAnns (Tyvar _ var) = Tyvar emptyAnn var

