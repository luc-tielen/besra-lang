
module X1.Parser.Module ( parser ) where

import Protolude
import X1.Types.Module
import X1.Types.Id
import X1.Parser.Helpers
import qualified X1.Parser.Scheme as Scheme


parser :: Parser (Module Decl)
parser = do
  void $ optional whitespace
  decls <- decl `endBy` whitespace
  eof
  pure $ Module decls

decl :: Parser Decl
decl = typeDecl <?> "type declaration"

typeDecl :: Parser Decl
typeDecl = withLineFold $ do
  var <- Id <$> lexeme' identifier <?> "variable"
  void . lexeme' $ char ':'
  TypeDecl var <$> Scheme.parser

