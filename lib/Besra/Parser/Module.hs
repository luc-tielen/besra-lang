
module Besra.Parser.Module ( parser, declParser ) where

import Protolude
import Besra.Types.IR1.Module
import Besra.Types.IR1.Expr
import qualified Besra.Parser.Expr as Expr
import qualified Besra.Parser.ADT as ADT
import qualified Besra.Parser.Trait as Trait
import qualified Besra.Parser.Impl as Impl
import Besra.Parser.Helpers
import Besra.Types.Ann


type Module' = Module Parsed
type Decl' = Decl Parsed

parser :: Parser Module'
parser = do
  void $ optional whitespace
  decls <- declParser `endBy` whitespace
  eof
  pure $ Module decls

declParser :: Parser Decl'
declParser = decl <?> "declaration"
  where
    decl =  dataDecl
        <|> traitDecl
        <|> implDecl
        <|> typeOrBindingDecl

typeOrBindingDecl :: Parser Decl'
typeOrBindingDecl = do
  result <- Expr.declParser
  case result of
    ExprTypeAnnDecl typeAnn -> pure $ TypeAnnDecl typeAnn
    ExprBindingDecl binding -> pure $ BindingDecl binding
    ExprFixityDecl fixity -> pure $ FixityDecl fixity

dataDecl :: Parser Decl'
dataDecl = DataDecl <$> ADT.parser

traitDecl :: Parser Decl'
traitDecl = TraitDecl <$> Trait.parser

implDecl :: Parser Decl'
implDecl = ImplDecl <$> Impl.parser

