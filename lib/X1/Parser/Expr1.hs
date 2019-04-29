
module X1.Parser.Expr1 ( parser, declParser ) where

import Protolude hiding ( try )
import X1.Types.Id
import X1.Types.Expr1
import X1.Parser.Helpers
import qualified X1.Parser.Lit as Lit
import qualified X1.Parser.Scheme as Scheme
import Text.Megaparsec.Debug


parser :: Parser Expr1
parser =  E1Lit <$> Lit.parser
      <|> ifParser  -- TODO try needed because of linefold?
      <|> letParser
      <|> varParser

varParser :: Parser Expr1
varParser = E1Var . Id <$> lexeme' identifier <?> "variable"

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
letParser = (try letBlockParser <|> letLineParser) <?> "let expression" where
  letBlockParser = do
    bindings <- withLineFold $ do
      keyword "let"
      indentation <- indentLevel
      let declParser' = dbg "indented decl" ( withIndent indentation declParser <?> "declaration")
      lexeme declParser' `sepBy1` (whitespace' <* notFollowedBy (keyword "in"))
    result <- withLineFold $ keyword "in" *> parser
    pure $ E1Let bindings result
  semiColon = lexeme' $ sameLine (char ';')
  singleLineDecl = lexeme declParser <?> "declaration"
  letLineParser = dbg "let line" $ withLineFold $ do
    keyword "let"
    bindings <- singleLineDecl `sepBy1` (semiColon <* notFollowedBy (keyword "in"))
    keyword "in"
    result <- parser
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

