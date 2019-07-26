
{-# LANGUAGE UndecidableInstances #-}

module Besra.Pass.IR1To2
  ( pass
  , PassState(..)
  ) where

import Protolude hiding ( Type, pass )
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import qualified Besra.Types.IR1 as IR1
import qualified Besra.Types.IR2 as IR2
import Besra.Types.Ann
import Besra.Types.Id

{-

This pass does a few transformations all in 1 go:
1. Removes information only relevant to parser / pretty printer (no longer useful)
   This includes things such as binary operations, parentheses, ...
2. Convert hex and binary numbers to decimal integers.
3. Removes data, trait, impl and fixity decls from the module AST.
   ADTs and trait/impl decls will be stored separately in compiler state.
   Fixity decls are no longer relevant after IR1
   (operator precedences have been resolved in a previous pass).
4. Converts some AST nodes to a more usable format for next phases of the compiler.
   In next phases, annotations will also play a heavier role (since structure
   will stay mostly the same); this also allows for reusable transforms
   like "foldAST" to be used more.

-}


data PassState (ph :: Phase)
  = PassState
  { adts :: [IR2.ADT ph]
  , traits :: [IR2.Trait ph]
  , impls :: [IR2.Impl ph]
  }

deriving instance Eq (Ann ph) => Eq (PassState ph)
deriving instance Show (Ann ph) => Show (PassState ph)

type PassM ph = State (PassState ph)

-- | Function that converts IR1 to IR2. Also returns the state of the pass
--   containing ADT, trait and impl decls for further use in the compiler.
pass :: IR1.Module ph -> (IR2.Module ph, PassState ph)
pass ast = runState (desugar ast) (PassState [] [] [])

-- | Typeclass that performs the heavy lifting of this pass.
--   The 2nd parameter ph is used for convincing the typesystem that
--   these conversions are possible for every phase in the compiler.
--   (This leads to more reuse and is more correct than hardcoding it
--   to 'Parsed since we can't touch the contents of the annotations
--   themselves now.)
class Desugarable a (ph :: Phase) where
  type Desugared a

  desugar :: a -> PassM ph (Desugared a)

instance Desugarable a ph => Desugarable [a] ph where
  type Desugared [a] = [Desugared a]

  desugar = traverse desugar

instance (Desugarable a ph, Desugarable b ph) => Desugarable (a, b) ph where
  type Desugared (a, b) = (Desugared a, Desugared b)

  desugar (a, b) = (,) <$> desugar a <*> desugar b

instance Desugarable (IR1.Module ph) ph where
  type Desugared (IR1.Module ph) = IR2.Module ph

  desugar (IR1.Module decls) =
    IR2.Module . catMaybes <$> desugar decls

instance Desugarable (IR1.ADT ph) ph where
  type Desugared (IR1.ADT ph) = IR2.ADT ph

  desugar (IR1.ADT sp hd constrs) = do
    hd'@(IR2.ADTHead _ ty) <- desugar hd
    constrs' <- traverse (desugarConDecl ty) constrs
    pure $ IR2.ADT sp hd' constrs'
    where
      arrow ann a =
        IR2.TApp (IR2.TApp (IR2.TCon (IR2.Tycon ann (Id "->"))) a)
      desugarConDecl ty (IR1.ConDecl ann name ts) = do
        ts' <- desugar ts
        let ty' = foldr (arrow ann) ty ts'
        pure $ IR2.ConDecl ann name ty'

instance Desugarable (IR1.ADTHead ph) ph where
  type Desugared (IR1.ADTHead ph) = IR2.ADTHead ph

  desugar (IR1.ADTHead con@(IR1.Tycon _ name) vars) = do
    con' <- desugar con
    vars' <- desugar vars
    let ty = foldl' IR2.TApp (IR2.TCon con') (IR2.TVar <$> vars')
    pure $ IR2.ADTHead name ty

instance Desugarable (IR1.Trait ph) ph where
  type Desugared (IR1.Trait ph) = IR2.Trait ph

  desugar (IR1.Trait ann ps p ts) =
    IR2.Trait ann <$> desugar ps <*> desugar p <*> desugar ts

instance Desugarable (IR1.Impl ph) ph where
  type Desugared (IR1.Impl ph) = IR2.Impl ph

  desugar (IR1.Impl ann ps p bindings) =
    IR2.Impl ann <$> desugar ps <*> desugar p <*> desugar bindings

instance Desugarable (IR1.Decl ph) ph where
  type Desugared (IR1.Decl ph) = Maybe (IR2.Decl ph)

  desugar = \case
    IR1.TypeAnnDecl typeAnn ->
      Just . IR2.TypeAnnDecl <$> desugar typeAnn
    IR1.BindingDecl binding ->
      Just . IR2.BindingDecl <$> desugar binding
    IR1.DataDecl adt -> do
      adt' <- desugar adt
      Nothing <$ modify (\s -> s { adts = adt' : adts s })
    IR1.TraitDecl trait -> do
      trait' <- desugar trait
      Nothing <$ modify (\s -> s { traits = trait' : traits s })
    IR1.ImplDecl impl -> do
      impl' <- desugar impl
      Nothing <$ modify (\s -> s { impls = impl' : impls s })
    _ -> pure Nothing

instance Desugarable (IR1.TypeAnn ph) ph where
  type Desugared (IR1.TypeAnn ph) = IR2.TypeAnn ph

  desugar (IR1.TypeAnn ann id scheme) =
    IR2.TypeAnn ann id <$> desugar scheme

instance Desugarable (IR1.Scheme ph) ph where
  type Desugared (IR1.Scheme ph) = IR2.Scheme ph

  desugar (IR1.Scheme ann ps t) =
    IR2.Scheme ann <$> desugar ps <*> desugar t

instance Desugarable (IR1.Pred ph) ph where
  type Desugared (IR1.Pred ph) = IR2.Pred ph

  desugar (IR1.IsIn ann id ts) =
    IR2.IsIn ann id <$> desugar ts

instance Desugarable (IR1.Type ph) ph where
  type Desugared (IR1.Type ph) = IR2.Type ph

  desugar = \case
    IR1.TCon tycon -> IR2.TCon <$> desugar tycon
    IR1.TVar tyvar -> IR2.TVar <$> desugar tyvar
    IR1.TApp t ts -> do
      let app = IR2.TApp
      t' <- desugar t
      ts' <- desugar ts
      pure $ foldl' app t' ts'
    IR1.TParen _ t -> desugar t

instance Desugarable (IR1.Tycon ph) ph where
  type Desugared (IR1.Tycon ph) = IR2.Tycon ph

  desugar (IR1.Tycon ann con) = pure $ IR2.Tycon ann con

instance Desugarable (IR1.Tyvar ph) ph where
  type Desugared (IR1.Tyvar ph) = IR2.Tyvar ph

  desugar (IR1.Tyvar ann con) = pure $ IR2.Tyvar ann con

instance Desugarable (IR1.Binding ph) ph where
  type Desugared (IR1.Binding ph) = IR2.Binding ph

  desugar (IR1.Binding ann id expr) =
    IR2.Binding ann id <$> desugar expr

instance Desugarable (IR1.Expr ph) ph where
  type Desugared (IR1.Expr ph) = IR2.Expr ph

  desugar = \case
    IR1.ELit ann lit -> IR2.ELit ann <$> desugar lit
    IR1.EVar ann var -> pure $ IR2.EVar ann var
    IR1.ECon ann var -> pure $ IR2.ECon ann var
    IR1.ELam ann args body ->
      IR2.ELam ann <$> desugar args <*> desugar body
    IR1.EApp ann f args -> do
      let app = IR2.EApp ann
      f' <- desugar f
      args' <- desugar args
      pure $ foldl' app f' args'
    IR1.EIf ann cnd tr fl ->
      IR2.EIf ann <$> desugar cnd <*> desugar tr <*> desugar fl
    IR1.ECase ann e clauses ->
      IR2.ECase ann <$> desugar e <*> desugar clauses
    IR1.ELet ann decls body ->
      IR2.ELet ann . catMaybes <$> desugar decls <*> desugar body
    IR1.EBinOp ann op l r -> do
      op' <- desugar op
      l' <- desugar l
      r' <- desugar r
      pure $ IR2.EApp ann (IR2.EApp ann op' l') r'
    IR1.ENeg ann e ->
      IR2.EApp ann (IR2.EVar ann $ Id "negate") <$> desugar e
    IR1.EParens _ e -> desugar e

instance Desugarable IR1.Pattern ph where
  type Desugared IR1.Pattern = IR2.Pattern

  desugar = \case
    IR1.PWildcard -> pure IR2.PWildcard
    IR1.PLit lit -> IR2.PLit <$> desugar lit
    IR1.PVar var -> pure $ IR2.PVar var
    IR1.PCon con pats -> IR2.PCon con <$> desugar pats
    IR1.PAs id pat -> IR2.PAs id <$> desugar pat

instance Desugarable (IR1.ExprDecl ph) ph where
  type Desugared (IR1.ExprDecl ph) = Maybe (IR2.Decl ph)

  desugar = \case
    IR1.ExprTypeAnnDecl typeAnn ->
      Just . IR2.TypeAnnDecl <$> desugar typeAnn
    IR1.ExprBindingDecl binding ->
      Just . IR2.BindingDecl <$> desugar binding
    IR1.ExprFixityDecl _ -> pure Nothing

instance Desugarable IR1.Lit ph where
  type Desugared IR1.Lit = IR2.Lit

  desugar = \case
    IR1.LChar c -> pure $ IR2.LChar c
    IR1.LString str -> IR2.LString <$> desugar str
    IR1.LNumber num -> IR2.LNumber <$> desugar num

instance Desugarable IR1.String ph where
  type Desugared IR1.String = IR2.String

  desugar (IR1.String str) = pure $ IR2.String str

instance Desugarable IR1.Number ph where
  type Desugared IR1.Number = IR2.Number

  desugar = \case
    IR1.SInt int -> pure $ IR2.Number int
    IR1.SHex hex -> pure . IR2.Number $ hexToInt hex
    IR1.SBin bin -> pure . IR2.Number $ binToInt bin

hexToInt :: Text -> Int
hexToInt = fromJust . readMaybe . T.unpack

binToInt :: Text -> Int
binToInt digits = go 0 (T.unpack $ T.drop 2 digits) where
  go = foldl' (\acc x -> acc * 2 + digitToInt x)
  digitToInt '0' = 0
  digitToInt '1' = 1
  digitToInt _ = panic "Unexpected digit when desugaring binary literal"
