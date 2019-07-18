
module Besra.Pass.StripParserInfo ( pass ) where

import Protolude hiding ( Type, pass )
import Control.Arrow ( (***) )
import Data.Maybe ( fromJust )
import qualified Data.Text as T
import qualified Besra.Types.IR1 as IR1
import Besra.Types.Ann
import Besra.Types.Id

{-

This pass does a few transformations all in 1 go:
1. Removes information only relevant to parser / pretty printer (no longer useful)
   This includes things such as binary operations, parentheses, ...
2. Convert hex and binary numbers to decimal integers.
3. Removes data, trait and impl decls from the AST, since that information
   is stored in the compiler state.
4. Converts some AST nodes to a more usable format for next phases of the compiler.
   In next phases, annotations will also play a heavier role (since structure
   will stay mostly the same); this also allows for reusable transforms
   like "foldAST" to be used.

-}

-- TODO move to separate files in IR2 folder
-- TODO common types? tyvar, tycon, ... ==> how to structure/organize types?

newtype String = String Text
  deriving (Eq, Show)

newtype Number = Number Int
  deriving (Eq, Show)

data Lit = LChar Char
         | LString String
         | LNumber Number
         deriving (Eq, Show)

data Pattern = PWildcard
             | PLit Lit
             | PVar Id
             | PCon Id [Pattern]
             | PAs Id Pattern
             deriving (Eq, Show)

data Tyvar (ph :: Phase)
  = Tyvar (Ann ph) Id

data Tycon (ph :: Phase)
  = Tycon (Ann ph) Id

data Type (ph :: Phase)
  = TCon (Tycon ph)
  | TVar (Tyvar ph)
  | TApp (Type ph) (Type ph)

data Pred (ph :: Phase)
  = IsIn (Ann ph) Id [Type ph]

data Scheme (ph :: Phase)
  = Scheme (Ann ph) [Pred ph] (Type ph)

data Expr (ph :: Phase)
  = ELit (Ann ph) Lit
  | EVar (Ann ph) Id
  | ECon (Ann ph) Id
  | ELam (Ann ph) [Pattern] (Expr ph)
  | EApp (Ann ph) (Expr ph) (Expr ph)
  | EIf (Ann ph) (Expr ph) (Expr ph) (Expr ph)
  | ECase (Ann ph) (Expr ph) [(Pattern, Expr ph)]
  | ELet (Ann ph) [Decl ph] (Expr ph)

data Binding (ph :: Phase)
  = Binding (Ann ph) Id (Expr ph)

data TypeAnn (ph :: Phase)
  = TypeAnn (Ann ph) Id (Scheme ph)

data Decl (ph :: Phase)
  = TypeAnnDecl (TypeAnn ph)
  | BindingDecl (Binding ph)

newtype Module (ph :: Phase)
  = Module [Decl ph]



pass :: IR1.Module ph -> Module ph
pass = desugar


class Desugarable a where
  type Desugared a

  desugar :: a -> Desugared a

instance Desugarable a => Desugarable [a] where
  type Desugared [a] = [Desugared a]

  desugar = map desugar

instance (Desugarable a, Desugarable b) => Desugarable (a, b) where
  type Desugared (a, b) = (Desugared a, Desugared b)

  desugar = desugar *** desugar

instance Desugarable (IR1.Module ph) where
  type Desugared (IR1.Module ph) = Module ph

  desugar (IR1.Module decls) =
    Module (catMaybes $ desugar decls)

instance Desugarable (IR1.Decl ph) where
  type Desugared (IR1.Decl ph) = Maybe (Decl ph)

  desugar = \case
    IR1.TypeAnnDecl typeAnn -> Just $ TypeAnnDecl $ desugar typeAnn
    IR1.BindingDecl binding -> Just $ BindingDecl $ desugar binding
    _ -> Nothing

{-
TODO what to do with the following:
- TraitDecl (Trait ph)
- ImplDecl (Impl ph)
-}

instance Desugarable (IR1.TypeAnn ph) where
  type Desugared (IR1.TypeAnn ph) = TypeAnn ph

  desugar (IR1.TypeAnn ann id scheme) =
    TypeAnn ann id (desugar scheme)

instance Desugarable (IR1.Scheme ph) where
  type Desugared (IR1.Scheme ph) = Scheme ph

  desugar (IR1.Scheme ann ps t) =
    Scheme ann (desugar ps) (desugar t)

instance Desugarable (IR1.Pred ph) where
  type Desugared (IR1.Pred ph) = Pred ph

  desugar (IR1.IsIn ann id ts) =
    IsIn ann id (desugar ts)

instance Desugarable (IR1.Type ph) where
  type Desugared (IR1.Type ph) = Type ph

  desugar = \case
    IR1.TCon tycon -> TCon (desugar tycon)
    IR1.TVar tyvar -> TVar (desugar tyvar)
    IR1.TApp t ts ->
      let app = TApp
          t' = desugar t
          ts' = map desugar ts
       in foldl' app t' ts'
    IR1.TParen _ t -> desugar t

instance Desugarable (IR1.Tycon ph) where
  type Desugared (IR1.Tycon ph) = Tycon ph

  desugar (IR1.Tycon ann con) = Tycon ann con

instance Desugarable (IR1.Tyvar ph) where
  type Desugared (IR1.Tyvar ph) = Tyvar ph

  desugar (IR1.Tyvar ann con) = Tyvar ann con

instance Desugarable (IR1.Binding ph) where
  type Desugared (IR1.Binding ph) = Binding ph

  desugar (IR1.Binding ann id expr) = Binding ann id (desugar expr)

instance Desugarable (IR1.Expr ph) where
  type Desugared (IR1.Expr ph) = Expr ph

  desugar = \case
    IR1.ELit ann lit -> ELit ann (desugar lit)
    IR1.EVar ann var -> EVar ann var
    IR1.ECon ann var -> ECon ann var
    IR1.ELam ann args body -> ELam ann (desugar args) (desugar body)
    IR1.EApp ann f args ->
      let app = EApp ann
          f' = desugar f
          args' = map desugar args
       in foldr app f' args'
    IR1.EIf ann cnd tr fl -> EIf ann (desugar cnd) (desugar tr) (desugar fl)
    IR1.ECase ann e clauses -> ECase ann (desugar e) (desugar clauses)
    IR1.ELet ann decls body -> ELet ann (catMaybes $ desugar decls) (desugar body)
    IR1.EBinOp ann op l r ->
      let op' = desugar op
          l' = desugar l
          r' = desugar r
      in EApp ann (EApp ann op' l') r'
    IR1.ENeg ann e -> EApp ann (EVar ann $ Id "negate") (desugar e)
    IR1.EParens _ e -> desugar e

instance Desugarable IR1.Pattern where
  type Desugared IR1.Pattern = Pattern

  desugar = \case
    IR1.PWildcard -> PWildcard
    IR1.PLit lit -> PLit (desugar lit)
    IR1.PVar var -> PVar var
    IR1.PCon con pats -> PCon con (desugar pats)
    IR1.PAs id pat -> PAs id (desugar pat)

instance Desugarable (IR1.ExprDecl ph) where
  type Desugared (IR1.ExprDecl ph) = Maybe (Decl ph)

  desugar = \case
    IR1.ExprTypeAnnDecl typeAnn -> Just $ TypeAnnDecl (desugar typeAnn)
    IR1.ExprBindingDecl binding -> Just $ BindingDecl (desugar binding)
    IR1.ExprFixityDecl _ -> Nothing

instance Desugarable IR1.Lit where
  type Desugared IR1.Lit = Lit

  desugar = \case
    IR1.LChar c -> LChar c
    IR1.LString str -> LString (desugar str)
    IR1.LNumber num -> LNumber (desugar num)

instance Desugarable IR1.String where
  type Desugared IR1.String = String

  desugar (IR1.String str) = String str

instance Desugarable IR1.Number where
  type Desugared IR1.Number = Number

  desugar = \case
    IR1.SInt int -> Number int
    IR1.SHex hex -> Number $ hexToInt hex
    IR1.SBin bin -> Number $ binToInt bin

hexToInt :: Text -> Int
hexToInt = fromJust . readMaybe . T.unpack

binToInt :: Text -> Int
binToInt digits = go 0 (T.unpack $ T.drop 2 digits) where
  go = foldl' (\acc x -> acc * 2 + digitToInt x)
  digitToInt '0' = 0
  digitToInt '1' = 1
  digitToInt _ = panic "Unexpected digit when desugaring binary literal"

