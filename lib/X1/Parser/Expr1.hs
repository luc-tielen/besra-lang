
module X1.Parser.Expr1 ( parser, expr, declParser ) where

import Protolude hiding ( try, functionName, Fixity, Prefix )
import Data.Char ( digitToInt )
import GHC.Unicode (isDigit)
import X1.Types.Id
import X1.Types.Ann
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
type AnnExpr1' = (Ann', Expr1')

-- TODO refactor possible with usage of 'span' once everything is annotated?
parser :: Parser Expr1'
parser = map snd expr

expr :: Parser AnnExpr1'
expr = makeExprParser term exprOperators <?> "expression"

exprOperators :: [[Operator Parser AnnExpr1']]
exprOperators =
  [ [ Prefix (lexeme' negateOp) ]
  , [ InfixL (lexeme' binOp) ]
  ]
  where
    -- TODO refactor
    binOp = do
      (opSpan, op) <- operatorParser
      pure $ \(span1', e1) (span2', e2) ->
        let sp = opSpan <> span1' <> span2'
         in (sp, E1BinOp sp op e1 e2)
    negateOp = do
      opSpan <- negateOpParser
      pure $ \(span', e) ->
        let sp = opSpan <> span'
         in (sp, E1Neg sp e)
    operatorParser = withSpan $ infixOp <|> infixFunction'
    negateOpParser = fst <$> withSpan (hidden $ char '-')
    infixOp = uncurry E1Var <$> withSpan (Id <$> opIdentifier)
    infixFunction' = infixFunction E1Var E1Con

term :: Parser AnnExpr1'
term = lexeme (withSpan term') <?> "expression" where
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
  funcName <- lexeme funcNameParser
  -- NOTE: next line is to prevent wrong order of parentheses in nested applications
  args <- some $ lexeme arg
  pure $ E1App funcName args
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
  (sp1, vars) <- withSpan $ lexeme' lambdaHead
  (sp2, body) <- expr
  pure $ E1Lam (sp1 <> sp2) vars body
  where
    lambdaHead = sameLine $ do
      void . lexeme $ char '\\'
      vars <- some $ lexeme Pattern.parser
      void $ lexeme (chunk "->" <?> "lambda arrow")
      pure vars

ifParser :: Parser Expr1'
ifParser = do
  keyword "if"
  cond <- lexeme' parser
  keyword "then"
  trueClause <- lexeme' parser
  keyword "else"
  E1If cond trueClause <$> parser

caseParser :: Parser Expr1'
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

letParser :: Parser Expr1'
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

declParser :: Parser ExprDecl'
declParser = withLineFold declParser' where
  declParser' =  try fixityDecl
             <|> try namedFunctionDecl
             <|> typeOrBindingDecl

fixityDecl :: Parser ExprDecl'
fixityDecl = do
  (sp1, fixityType) <- withSpan $ lexeme' fixityTypeParser
  precedence <- withDefault 9 $ digitToInt <$> lexeme' decimal
  (sp2, operator) <- lexeme (withSpan $ Id <$> opIdentifier <|> infixFunction')
  pure $ ExprFixityDecl (sp1 <> sp2) fixityType precedence operator
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
  (sp1, (funcName, vars)) <- withSpan $ lexeme' functionHead
  (sp2, expr') <- expr
  let body = E1Lam (sp1 <> sp2) vars expr'
  pure $ ExprBindingDecl $ Binding (sp1 <> sp2) funcName body
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
  (varSpan, var) <- lexeme' $ withSpan declIdentifier
  separator <- lexeme' $ typeSeparator <|> assign
  case separator of
    ':' -> ExprTypeAnnDecl . TypeAnn var <$> Scheme.parser
    '=' -> do
      (exprSpan, e) <- expr
      pure $ ExprBindingDecl $ Binding (varSpan <> exprSpan) var e
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

