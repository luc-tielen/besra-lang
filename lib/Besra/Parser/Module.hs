
module Besra.Parser.Module ( parser ) where

import Protolude
import Besra.Types.IR1.Module
import Besra.Types.IR1.Expr
import qualified Besra.Parser.Expr as Expr
import qualified Besra.Parser.ADT as ADT
import qualified Besra.Parser.Trait as Trait
import qualified Besra.Parser.Impl as Impl
import Besra.Parser.Helpers
import Besra.Types.Ann


type Module' = Module 'Parsed
type Decl' = Decl 'Parsed

parser :: Parser Module'
parser = do
  void $ optional whitespace
  decls <- decl `endBy` whitespace
  eof
  pure $ Module decls

decl :: Parser Decl'
decl = decl' <?> "declaration"
  where
    decl' =  dataDecl
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
