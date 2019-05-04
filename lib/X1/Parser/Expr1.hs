
module X1.Parser.Expr1 ( parser, declParser ) where

import Protolude hiding ( try )
import X1.Types.Id
import X1.Types.Expr1
import X1.Parser.Helpers
import qualified X1.Parser.Lit as Lit
import qualified X1.Parser.Scheme as Scheme


parser :: Parser Expr1
parser = parser' <?> "expression" where
  parser' =  E1Lit <$> Lit.parser
         <|> lineFoldedExprs
         <|> letParser
         <|> try funcParser
         <|> varParser
         <|> betweenParens parser
  lineFoldedExprs = withLineFold $ lamParser <|> ifParser

funcParser :: Parser Expr1
funcParser = funcParser' <?> "function application" where
  -- NOTE: next line is to prevent wrong order of parentheses in nested applications
  funcNameParser = E1Var . Id <$> lexeme (identifier <|> capitalIdentifier)
  funcParser' = sameLine $ do
    funcName <- funcNameParser
    args <- some $ lexeme (funcNameParser <|> parser)
    pure $ E1App funcName args

varParser :: Parser Expr1
varParser = E1Var . Id <$> lexeme identifier <?> "variable"

lamParser :: Parser Expr1
lamParser = lamParser' <?> "lambda expression" where
  lambdaHead = sameLine $ do
    void . lexeme $ char '\\'
    vars <- some $ lexeme arg
    void $ lexeme (chunk "->" <?> "lambda arrow")
    pure vars
  lamParser' = do
    vars <- lexeme' lambdaHead
    body <- lexeme parser
    pure $ E1Lam vars body

-- TODO pattern
arg :: Parser Id
arg = Id <$> identifier <?> "variable"

ifParser :: Parser Expr1
ifParser = ifParser' <?> "if expression" where
  ifParser' = do
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
      lexeme declParser' `sepBy1` whitespace'
    result <- withLineFold $ (keyword "in" <?> inLabel) *> parser
    pure $ E1Let bindings result

declParser :: Parser ExprDecl
declParser = withLineFold declParser' where
  declParser' = try namedFunctionDecl <|> typeOrBindingDecl
  assign = char '=' <?> "rest of assignment"
  typeSeparator = char ':' <?> "rest of type declaration"

  functionHead = sameLine $ do
    funcName <- Id <$> lexeme identifier
    vars <- some $ lexeme arg
    void $ lexeme assign
    pure (funcName, vars)
  namedFunctionDecl = do
    (funcName, vars) <- lexeme' functionHead
    body <- E1Lam vars <$> parser
    pure $ ExprBindingDecl funcName body

  typeOrBindingDecl = do
    var <- Id <$> lexeme' identifier <?> "variable"
    separator <- lexeme' $ typeSeparator <|> assign
    case separator of
      ':' -> ExprTypeDecl var <$> Scheme.parser
      '=' -> ExprBindingDecl var <$> parser
      _ -> panic "Parse error when parsing declaration."

