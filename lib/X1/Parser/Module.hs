
module X1.Parser.Module ( parser ) where

import Protolude
import X1.Types.Module
import X1.Types.Id
import X1.Parser.Helpers
import qualified X1.Parser.Scheme as Scheme
import qualified X1.Parser.Expr1 as Expr1


parser :: Parser (Module Decl)
parser = do
  void $ optional whitespace
  decls <- decl `endBy` whitespace
  eof
  pure $ Module decls

decl :: Parser Decl
decl = typeOrBindingDecl <?> "type or binding declaration"

typeOrBindingDecl :: Parser Decl
typeOrBindingDecl = withLineFold $ do
  var <- Id <$> lexeme' identifier <?> "variable"
  separator <- lexeme' $  (char ':' <?> "rest of type declaration")
                      <|> (char '=' <?> "rest of assignment")
  case separator of
    ':' -> TypeDecl var <$> Scheme.parser
    '=' -> BindingDecl var <$> Expr1.parser
    _ -> panic "Parse error when parsing top level declaration."

