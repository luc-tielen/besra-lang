
module X1.Parser.Expr1 ( parser, declParser ) where

import Protolude hiding ( try, functionName, Fixity )
import Data.Char ( digitToInt )
import GHC.Unicode (isDigit)
import X1.Types.Id
import X1.Types.Fixity
import X1.Types.Expr1.Expr
import X1.Types.Expr1.TypeAnn
import X1.Parser.Helpers
import Control.Monad.Combinators.Expr
import qualified X1.Parser.Lit as Lit
import qualified X1.Parser.Scheme as Scheme
import qualified X1.Parser.Pattern as Pattern


parser :: Parser Expr1
parser = expr

expr :: Parser Expr1
expr = makeExprParser term exprOperators <?> "expression"

exprOperators :: [[Operator Parser Expr1]]
exprOperators = [ [ InfixL (E1BinOp <$> lexeme' operatorParser) ] ]
  where
    operatorParser =  infixOp <|> infixFunction'
    infixOp = E1Var . Id <$> opIdentifier
    infixFunction' = infixFunction (E1Var . Id) (E1Con . Id)

term :: Parser Expr1
term = term' <?> "expression" where
  -- TODO move lexeme to start of term'?
  term' =  lexeme litParser
       <|> withLineFold lineFoldedExprs
       <|> letParser
       <|> try applyFuncParser
       <|> varParser
       <|> conParser
       <|> parens parser
  lineFoldedExprs =  lamParser
                 <|> ifParser
                 <|> caseParser

litParser :: Parser Expr1
litParser = E1Lit <$> Lit.parser

applyFuncParser :: Parser Expr1
applyFuncParser = sameLine $ do
  funcName <- lexeme funcNameParser
  -- NOTE: next line is to prevent wrong order of parentheses in nested applications
  args <- some $ lexeme arg
  pure $ E1App funcName args
  where
    variable = E1Var . Id <$> identifier
    constructor = E1Con . Id <$> capitalIdentifier
    prefixOp = E1Var <$> prefixOperator
    funcNameParser =  variable
                  <|> constructor
                  <|> try prefixOp
                  <|> parens parser
    arg =  litParser
       <|> varParser
       <|> conParser
       <|> parens parser

varParser :: Parser Expr1
varParser = E1Var . Id <$> varParser' where
  varParser' = lexeme (try opVar <|> identifier)
  opVar = betweenParens opIdentifier

conParser :: Parser Expr1
conParser = E1Con . Id <$> lexeme capitalIdentifier

lamParser :: Parser Expr1
lamParser = do
  vars <- lexeme' lambdaHead
  body <- lexeme parser
  pure $ E1Lam vars body
  where
    lambdaHead = sameLine $ do
      void . lexeme $ char '\\'
      vars <- some $ lexeme Pattern.parser
      void $ lexeme (chunk "->" <?> "lambda arrow")
      pure vars

ifParser :: Parser Expr1
ifParser = do
  keyword "if"
  cond <- lexeme' parser
  keyword "then"
  trueClause <- lexeme' parser
  keyword "else"
  E1If cond trueClause <$> parser

caseParser :: Parser Expr1
caseParser = do
  keyword "case"
  expr' <- lexeme' parser
  keyword "of"
  indentation <- indentLevel
  let clauseParser' = withIndent indentation clauseParser <?> "case clause"
  clauses <- some clauseParser'
  notFollowedBy clauseParser <?> "properly indented case clause"
  pure $ E1Case expr' clauses
  where
    clauseParser = withLineFold $ do
      pat <- lexeme' Pattern.parser
      void . lexeme' $ chunk "->"
      expr' <- parser
      pure (pat, expr')

letParser :: Parser Expr1
letParser = do
  bindings <- withLineFold $ do
    keyword "let"
    indentation <- indentLevel
    let declParser' = withIndent indentation declParser <?> "declaration"
    lexeme declParser' `sepBy1` whitespace'
  result <- withLineFold $ (keyword "in" <?> inLabel) *> parser
  pure $ E1Let bindings result
  where
    inLabel = "properly indented declaration or 'in' keyword"

declParser :: Parser ExprDecl
declParser = withLineFold declParser' where
  declParser' =  try fixityDecl
             <|> try namedFunctionDecl
             <|> typeOrBindingDecl

fixityDecl :: Parser ExprDecl
fixityDecl =  do
  fixityType <- lexeme' fixityTypeParser
  precedence <- withDefault 9 $ digitToInt <$> lexeme' decimal
  operator <- Id <$> lexeme (opIdentifier <|> infixFunction')
  pure $ ExprFixityDecl fixityType precedence operator
  where
    fixityTypeParser =  keyword "infixl" $> L
                    <|> keyword "infixr" $> R
                    <|> keyword "infix" $> M
    digit = satisfy isDigit
    precedenceMsg = "precedence between 0..9"
    decimal = do
      parsed <- digit <?> precedenceMsg
      notFollowedBy digit <?> precedenceMsg
      pure parsed
    infixFunction' = infixFunction identity identity

namedFunctionDecl :: Parser ExprDecl
namedFunctionDecl = do
  (funcName, vars) <- lexeme' functionHead
  body <- E1Lam vars <$> parser
  pure $ ExprBindingDecl $ Binding funcName body
  where
    functionName =  Id <$> identifier
                <|> prefixOperator
    functionHead = sameLine $ do
      funcName <- lexeme functionName
      vars <- some $ lexeme Pattern.parser
      void $ lexeme assign
      pure (funcName, vars)

typeOrBindingDecl :: Parser ExprDecl
typeOrBindingDecl = do
  var <- lexeme' declIdentifier
  separator <- lexeme' $ typeSeparator <|> assign
  case separator of
    ':' -> ExprTypeAnnDecl . TypeAnn var <$> Scheme.parser
    '=' -> ExprBindingDecl . Binding var <$> parser
    _ -> panic "Parse error when parsing declaration."
  where
    declIdentifier = declVar <|> prefixOperator
    declVar = Id <$> identifier <?> "variable"
    typeSeparator = char ':' <?> "rest of type declaration"

prefixOperator :: Parser Id
prefixOperator = Id <$> sameLine (betweenParens opIdentifier) <?> "operator"

assign :: Parser Char
assign = char '=' <?> "rest of assignment"

infixFunction :: (Text -> a) -> (Text -> a) -> Parser a
infixFunction var con =
  betweenBackticks $  var <$> infixFunc
                  <|> con <$> infixCon
  where
    infixFunc = identifier <?> "infix function"
    infixCon = capitalIdentifier <?> "infix constructor"
    betweenBackticks = between (backtick <?> "operator") backtick
    backtick = char '`'

parens :: Parser Expr1 -> Parser Expr1
parens p = E1Parens <$> lexeme (betweenParens p)

