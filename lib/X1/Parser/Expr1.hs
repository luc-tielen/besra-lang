
module X1.Parser.Expr1 ( parser, declParser ) where

import Protolude hiding ( try )
import X1.Types.Id
import X1.Types.Expr1
import X1.Parser.Helpers
import qualified X1.Parser.Lit as Lit
import qualified X1.Parser.Scheme as Scheme
--import Text.Megaparsec.Debug


parser :: Parser Expr1
parser =  E1Lit <$> Lit.parser
      <|> lamParser
      <|> ifParser  -- TODO try needed because of linefold?
      <|> letParser
      <|> varParser

varParser :: Parser Expr1
varParser = E1Var . Id <$> lexeme' identifier <?> "variable"

lamParser :: Parser Expr1
lamParser = lamParser' <?> "lambda expression" where
  lamParser' = do
    void . lexeme' $ char '\\'
    -- TODO vars on same line
    vars <- lexeme arg `sepBy1` whitespace'
    void $ lexeme' (chunk "->" <?> "lambda arrow")
    body <- lexeme parser
    pure $ E1Lam vars body

-- TODO pattern
arg :: Parser Id
arg = Id <$> identifier <?> "variable"

ifParser :: Parser Expr1
ifParser = ifParser' <?> "if expression" where
  ifParser' = withLineFold $ do
    keyword "if"
    cond <- lexeme' parser
    keyword "then"
    trueClause <- lexeme' parser
    keyword "else"
    E1If cond trueClause <$> lexeme' parser

letParser :: Parser Expr1
letParser = letParser' <?> "let expression" where
  inLabel = "properly indented declaration or 'in' keyword"
  letParser' = do
    bindings <- withLineFold $ do
      keyword "let"
      indentation <- indentLevel
      let declParser' = withIndent indentation declParser <?> "declaration"
      lexeme declParser' `sepBy1` (whitespace' <* notFollowedBy (keyword "in"))
    result <- withLineFold $ (keyword "in" <?> inLabel) *> parser
    pure $ E1Let bindings result

declParser :: Parser ExprDecl
declParser = withLineFold $ do
  var <- Id <$> lexeme' identifier <?> "variable"
  separator <- lexeme' $  (char ':' <?> "rest of type declaration")
                      <|> (char '=' <?> "rest of assignment")
  case separator of
    ':' -> ExprTypeDecl var <$> Scheme.parser
    '=' -> ExprBindingDecl var <$> parser
    _ -> panic "Parse error when parsing declaration."

