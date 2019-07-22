
{-# OPTIONS_GHC -Wno-orphans #-}

module Besra.PrettyPrinter ( prettyPrint ) where

import Protolude
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Besra.Types.IR1 as IR1
import Besra.Types.Fixity
import Besra.Types.Id
import Besra.Parser.Helpers ( isOperatorChar )


prettyPrint :: Pretty a => a -> Text
prettyPrint =
  renderStrict . layoutSmart defaultLayoutOptions . pretty

indentation :: Int
indentation = 2

prettyBlock :: Pretty a => [a] -> Doc ann
prettyBlock = indentBlock . vsep . map pretty

indentBlock :: Doc ann -> Doc ann
indentBlock block = nest indentation (hardline <> block)


instance Pretty (IR1.Module ph) where
  pretty (IR1.Module decls) = vsep $ pretty <$> decls

instance Pretty (IR1.Decl ph) where
  pretty = \case
    IR1.FixityDecl fixityInfo -> pretty fixityInfo
    IR1.BindingDecl binding -> pretty binding
    IR1.TypeAnnDecl typeAnn -> pretty typeAnn
    IR1.DataDecl adt -> pretty adt
    IR1.TraitDecl trait -> pretty trait
    IR1.ImplDecl impl -> pretty impl

instance Pretty (IR1.Trait ph) where
  pretty (IR1.Trait _ ps p typeAnns) =
    case (null ps, null typeAnns) of
      (True, True) ->
        "trait" <+> pretty p <+> "where"
      (False, True) ->
        "trait"
          <+> printConstraints ps
          <+> pretty p
          <+> "where"
      (True, False) ->
        "trait" <+> pretty p <+> "where"
          <> prettyBlock typeAnns
      (False, False) ->
        "trait"
          <+> printConstraints ps
          <+> pretty p
          <+> "where"
          <> prettyBlock typeAnns

instance Pretty (IR1.Impl ph) where
  pretty (IR1.Impl _ ps p bindings) =
    case (null ps, null bindings) of
      (True, True) ->
        "impl" <+> pretty p <+> "where"
      (True, False) ->
        "impl" <+> pretty p <+> "where"
          <> prettyBlock bindings
      (False, True) ->
        "impl" <+> printConstraints ps <+> pretty p <+> "where"
      (False, False) ->
        "impl" <+> printConstraints ps <+> pretty p <+> "where"
          <> prettyBlock bindings

instance Pretty (IR1.ADT ph) where
  pretty (IR1.ADT _ hd bodies) =
    if null bodies
      then "data" <+> pretty hd
      else "data" <+> pretty hd <+> "="
              <+> hcat (punctuate " | " $ pretty <$> bodies)

instance Pretty (IR1.ADTHead ph) where
  pretty (IR1.ADTHead con vars) =
    if null vars
      then pretty con
      else pretty con <+> hsep (pretty <$> vars)

instance Pretty (IR1.ConDecl ph) where
  pretty (IR1.ConDecl _ con tys) =
    if null tys
      then pretty con
      else pretty con <+> hsep (pretty <$> tys)

instance Pretty (IR1.FixityInfo ph) where
  pretty (IR1.FixityInfo _ fixity prio op) =
    let
      keyword = case fixity of
        L -> "infixl"
        R -> "infixr"
        M -> "infix"
     in keyword <+> pretty prio <+> pretty op

instance Pretty (IR1.TypeAnn ph) where
  pretty (IR1.TypeAnn _ var@(Id var') ty) =
    prettyVar <+> ":" <+> pretty ty
    where prettyVar =
            if isOperatorId var'
              then parens $ pretty var
              else pretty var

instance Pretty (IR1.Scheme ph) where
  pretty (IR1.Scheme _ ps ty) =
    if null ps
      then pretty ty
      else printConstraints ps <+> pretty ty

instance Pretty (IR1.Pred ph) where
  pretty (IR1.IsIn _ name tys) =
    pretty name <+> hsep (prettyType <$> tys)
    where prettyType ty@(IR1.TApp _ _) = parens (pretty ty)
          prettyType ty = pretty ty

printConstraints :: [IR1.Pred ph] -> Doc ann
printConstraints [] = ""
printConstraints [p] = pretty p <+> "=>"
printConstraints ps =
  parens (hsep $ punctuate comma $ pretty <$> ps) <+> "=>"

pattern TArrCon ann = (IR1.TCon (IR1.Tycon ann (Id "->")))
pattern TArrow ann t1 t2 <- IR1.TApp (TArrCon ann) [t1, t2] where
  TArrow ann t1 t2 = IR1.TApp (TArrCon ann) [t1, t2]

instance Pretty (IR1.Type ph) where
  pretty = \case
    IR1.TCon tycon -> pretty tycon
    IR1.TVar tyvar -> pretty tyvar
    TArrow _ t1 t2 -> pretty t1 <+> "->" <+> pretty t2
    IR1.TApp ty tys ->
      pretty ty <+> hsep (pretty <$> tys)
    IR1.TParen _ ty -> parens $ pretty ty

instance Pretty (IR1.Tycon ph) where
  pretty (IR1.Tycon _ con) = pretty con

instance Pretty (IR1.Tyvar ph) where
  pretty (IR1.Tyvar _ var) = pretty var

instance Pretty (IR1.Binding ph) where
  pretty (IR1.Binding _ var@(Id var') expr) =
    case expr of
      IR1.ELam _ pats body ->
        prettyVar <+> hsep (pretty <$> pats) <+> "=" <+> pretty body
      _ ->
        prettyVar <+> "=" <+> pretty expr
    where prettyVar =
            if isOperatorId var'
              then parens $ pretty var
              else pretty var

instance Pretty (IR1.Expr ph) where
  pretty = \case
    IR1.ELit _ lit -> pretty lit
    IR1.ELam _ pats body ->
      "\\" <> hsep (pretty <$> pats) <+> "->" <+> pretty body
      -- TODO indent for long lines, try single line if possible (grouping)
    IR1.EApp _ f args ->
      case f of
        (IR1.EVar _ (Id f')) | isOperatorId f' ->
          parens (pretty f) <+> hsep (pretty <$> args)
        _ ->
          pretty f <+> hsep (pretty <$> args)
    IR1.EBinOp _ op l r -> pretty l <+> pretty op <+> pretty r
    IR1.ENeg _ e -> "-" <> pretty e
    IR1.EVar _ var -> pretty var
    IR1.ECon _ con -> pretty con
    IR1.EIf _ cnd tr fl -> indentBlock ifBlock
      where ifBlock = "if" <+> pretty cnd
              <> indentBlock (vsep [ "then" <+> pretty tr
                                    , "else" <+> pretty fl
                                    ])
    IR1.ECase _ e clauses ->
      "case" <+> pretty e <+> "of"
        <> indentBlock (vcat $ prettyClause <$> clauses)
    IR1.EParens _ e -> parens $ pretty e
    IR1.ELet _ decls body ->
      indentBlock letDoc
      where letDoc = "let" <> indentBlock (vcat $ pretty <$> decls) <> hardline
                  <> "in" <> indentBlock (pretty body)

prettyClause :: (IR1.Pattern, IR1.Expr ph) -> Doc ann
prettyClause (pat, expr) =
  pretty pat <+> "->" <> indentBlock (pretty expr)


instance Pretty (IR1.ExprDecl ph) where
  pretty = \case
    IR1.ExprTypeAnnDecl typeAnn -> pretty typeAnn
    IR1.ExprBindingDecl binding -> pretty binding
    IR1.ExprFixityDecl fixityInfo -> pretty fixityInfo

instance Pretty IR1.Pattern where
  pretty = \case
    IR1.PWildcard -> "_"
    IR1.PLit lit -> pretty lit
    IR1.PVar var -> pretty var
    IR1.PCon con pats ->
      if null pats
        then pretty con
        else parens $ pretty con <+> hsep (pretty <$> pats)
    IR1.PAs var pat -> pretty var <> "@" <> pretty pat

instance Pretty IR1.Lit where
  pretty = \case
    IR1.LNumber num -> pretty num
    IR1.LString str -> pretty str
    IR1.LChar c -> squotes $ pretty c

instance Pretty IR1.Number where
  pretty = \case
    IR1.SInt int -> pretty int
    IR1.SHex hex -> pretty hex
    IR1.SBin bin -> pretty bin

instance Pretty IR1.String where
  pretty (IR1.String str) = dquotes $ pretty str

instance Pretty Id where
  pretty (Id x) = pretty x

isOperatorId :: Text -> Bool
isOperatorId = isOperatorChar . T.head

