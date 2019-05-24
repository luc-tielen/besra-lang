
module X1.Parser.Module ( parser ) where

import Protolude
import X1.Types.Module
import X1.Types.Expr1
import qualified X1.Parser.Expr1 as Expr1
import qualified X1.Parser.ADT as ADT
import X1.Parser.Helpers


parser :: Parser (Module Decl)
parser = do
  void $ optional whitespace
  decls <- decl `endBy` whitespace
  eof
  pure $ Module decls

decl :: Parser Decl
decl = decl' <?> "declaration"
  where
    decl' = dataDecl <|> typeOrBindingDecl

typeOrBindingDecl :: Parser Decl
typeOrBindingDecl = do
  result <- Expr1.declParser
  case result of
    ExprTypeDecl id scheme -> pure $ TypeDecl id scheme
    ExprBindingDecl id expr -> pure $ BindingDecl id expr
    ExprFixityDecl fixity precedence op -> pure $ FixityDecl fixity precedence op

dataDecl :: Parser Decl
dataDecl = DataDecl <$> ADT.parser

