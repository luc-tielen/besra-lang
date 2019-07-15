
module X1.Parser.Expr1 ( parser, declParser ) where

import Protolude hiding ( try, functionName, Fixity, Prefix )
import Data.Char ( digitToInt )
import Data.Maybe ( fromJust )
import GHC.Unicode (isDigit)
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span
import X1.Types.Fixity
import X1.Types.Expr1.Expr
import X1.Types.Expr1.TypeAnn
import X1.Parser.Helpers
import Control.Monad.Combinators.Expr
import qualified X1.Parser.Lit as Lit
import qualified X1.Parser.Scheme as Scheme
import qualified X1.Parser.Pattern as Pattern


type Expr1' = Expr1 'Parsed
type ExprDecl' = ExprDecl 'Parsed
type Ann' = Ann 'Parsed

parser :: Parser Expr1'
parser = expr

expr :: Parser Expr1'
expr = makeExprParser term exprOperators <?> "expression"

exprOperators :: [[Operator Parser Expr1']]
exprOperators =
  [ [ Prefix (lexeme' negateOp) ]
  , [ InfixL (lexeme' binOp) ]
  ]
  where
    binOp = do
      op <- operatorParser
      pure $ \e1 e2 ->
        let sp = span e1 <> span e2
         in E1BinOp sp op e1 e2
    negateOp = do
      opSpan <- negateOpParser
      pure $ \ e ->
        let sp = opSpan <> span e
         in E1Neg sp e
    operatorParser = infixOp <|> infixFunction'
    negateOpParser = fst <$> withSpan (hidden $ char '-')
    infixOp = uncurry E1Var <$> withSpan (Id <$> opIdentifier)
    infixFunction' = infixFunction E1Var E1Con

term :: Parser Expr1'
term = lexeme term' <?> "expression" where
  term' =  litParser
       <|> withLineFold lineFoldedExprs
       <|> letParser
       <|> try applyFuncParser
       <|> varParser
       <|> conParser
       <|> parens parser
  lineFoldedExprs =  lamParser
                 <|> ifParser
                 <|> caseParser

litParser :: Parser Expr1'
litParser = uncurry E1Lit <$> withSpan Lit.parser

applyFuncParser :: Parser Expr1'
applyFuncParser = sameLine $ do
  startPos <- getOffset
  funcName <- lexeme funcNameParser
  -- NOTE: next line is to prevent wrong order of parentheses in nested applications
  args <- some $ lexeme arg
  let sp = span $ fromJust $ nonEmpty args
  pure $ E1App (startPos .> sp) funcName args
  where
    variable = uncurry E1Var <$> withSpan (Id <$> identifier)
    constructor = uncurry E1Con <$> withSpan (Id <$> capitalIdentifier)
    prefixOp = uncurry E1Var <$> withSpan prefixOperator
    funcNameParser =  variable
                  <|> constructor
                  <|> try prefixOp
                  <|> parens parser
    arg =  litParser
       <|> varParser
       <|> conParser
       <|> parens parser

varParser :: Parser Expr1'
varParser = uncurry E1Var <$> varParser' where
  varParser' = withSpan $ Id <$> (try opVar <|> identifier)
  opVar = betweenParens opIdentifier

conParser :: Parser Expr1'
conParser =
  uncurry E1Con <$> withSpan (Id <$> capitalIdentifier)

lamParser :: Parser Expr1'
lamParser = do
  startPos <- getOffset
  vars <- lexeme' lambdaHead
  body <- parser
  pure $ E1Lam (startPos .> span body) vars body
  where
    lambdaHead = sameLine $ do
      void . lexeme $ char '\\'
      vars <- some $ lexeme Pattern.parser
      void $ lexeme (chunk "->" <?> "lambda arrow")
      pure vars

ifParser :: Parser Expr1'
ifParser = do
  startPos <- getOffset
  keyword "if"
  cond <- lexeme' parser
  keyword "then"
  trueClause <- lexeme' parser
  keyword "else"
  falseClause <- parser
  pure $ E1If (startPos .> span falseClause) cond trueClause falseClause

caseParser :: Parser Expr1'
caseParser = do
  startPos <- getOffset
  keyword "case"
  expr' <- lexeme' parser
  keyword "of"
  indentation <- indentLevel
  let clauseParser' = withIndent indentation clauseParser <?> "case clause"
  clauses <- some clauseParser'
  notFollowedBy clauseParser <?> "properly indented case clause"
  let sp = span $ fromJust $ nonEmpty clauses
  pure $ E1Case (startPos .> sp) expr' clauses
  where
    clauseParser = withLineFold $ do
      pat <- lexeme' Pattern.parser
      void . lexeme' $ chunk "->"
      expr' <- parser
      pure (pat, expr')

letParser :: Parser Expr1'
letParser = do
  (startPos, bindings) <- withLineFold $ do
    startPos <- getOffset
    keyword "let"
    indentation <- indentLevel
    let declParser' = withIndent indentation declParser <?> "declaration"
    decls <- lexeme declParser' `sepBy1` whitespace'
    pure (startPos, decls)
  result <- withLineFold $ (keyword "in" <?> inLabel) *> parser
  pure $ E1Let (startPos .> span result) bindings result
  where
    inLabel = "properly indented declaration or 'in' keyword"

declParser :: Parser ExprDecl'
declParser = withLineFold declParser' where
  declParser' =  try fixityDecl
             <|> try namedFunctionDecl
             <|> typeOrBindingDecl

fixityDecl :: Parser ExprDecl'
fixityDecl = do
  startPos <- getOffset
  fixityType <- lexeme' fixityTypeParser
  precedence <- withDefault 9 $ digitToInt <$> lexeme' decimal
  (sp, operator) <- lexeme (withSpan $ Id <$> opIdentifier <|> infixFunction')
  let fixity = FixityInfo (startPos .> sp) fixityType precedence operator
  pure $ ExprFixityDecl fixity
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
    infixFunction' = infixFunction (flip const) (flip const)

namedFunctionDecl :: Parser ExprDecl'
namedFunctionDecl = do
  startPos <- getOffset
  (funcName, vars) <- lexeme' functionHead
  expr' <- parser
  let sp = span expr'
      body = E1Lam (startPos .> sp) vars expr'
  pure $ ExprBindingDecl $ Binding (startPos .> sp) funcName body
  where
    functionName =  Id <$> identifier
                <|> prefixOperator
    functionHead = sameLine $ do
      funcName <- lexeme functionName
      vars <- some $ lexeme Pattern.parser
      void $ lexeme assign
      pure (funcName, vars)

typeOrBindingDecl :: Parser ExprDecl'
typeOrBindingDecl = do
  startPos <- getOffset
  var <- lexeme' declIdentifier
  separator <- lexeme' $ typeSeparator <|> assign
  case separator of
    ':' -> do
      scheme <- Scheme.parser
      pure $ ExprTypeAnnDecl $ TypeAnn (startPos .> span scheme) var scheme
    '=' -> do
      e <- expr
      pure $ ExprBindingDecl $ Binding (startPos .> span e) var e
    _ -> panic "Parse error when parsing declaration."
  where
    declIdentifier = declVar <|> prefixOperator
    declVar = Id <$> identifier <?> "variable"
    typeSeparator = char ':' <?> "rest of type declaration"

prefixOperator :: Parser Id
prefixOperator = Id <$> sameLine (betweenParens opIdentifier) <?> "operator"

assign :: Parser Char
assign = char '=' <?> "rest of assignment"

data InfixParseResult = InfixFunc | InfixCon

infixFunction :: (Ann' -> Id -> a) -> (Ann' -> Id -> a) -> Parser a
infixFunction var con = do
  (ann, (which, parsed)) <- withSpan $ betweenBackticks infixFuncOrCon
  case which of
    InfixFunc -> pure $ var ann parsed
    InfixCon -> pure $ con ann parsed
  where
    infixFuncOrCon =  (InfixFunc,) <$> infixFunc <|> (InfixCon,) <$> infixCon
    infixFunc = Id <$> identifier <?> "infix function"
    infixCon = Id <$> capitalIdentifier <?> "infix constructor"
    betweenBackticks = between (backtick <?> "operator") backtick
    backtick = char '`'

parens :: Parser Expr1' -> Parser Expr1'
parens p =
  uncurry E1Parens <$> withSpan (betweenParens p)

