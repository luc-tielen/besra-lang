
module Besra.Parser.Expr ( parser, declParser ) where

import Protolude hiding ( try, functionName, Fixity, Prefix )
import Data.Char ( digitToInt )
import Data.Maybe ( fromJust )
import GHC.Unicode (isDigit)
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.Fixity
import Besra.Types.IR1.Expr
import Besra.Types.IR1.TypeAnn
import Besra.Parser.Helpers
import Control.Monad.Combinators.Expr
import qualified Besra.Parser.Lit as Lit
import qualified Besra.Parser.Scheme as Scheme
import qualified Besra.Parser.Pattern as Pattern


type Expr' = Expr 'Parsed
type ExprDecl' = ExprDecl 'Parsed
type Ann' = Ann 'Parsed

parser :: Parser Expr'
parser = expr

expr :: Parser Expr'
expr = makeExprParser term exprOperators <?> "expression"

exprOperators :: [[Operator Parser Expr']]
exprOperators =
  [ [ Prefix (lexeme' negateOp) ]
  , [ InfixL (lexeme' binOp) ]
  ]
  where
    binOp = do
      op <- operatorParser
      pure $ \e1 e2 ->
        let sp = span e1 <> span e2
         in EBinOp sp op e1 e2
    negateOp = do
      opSpan <- negateOpParser
      pure $ \ e ->
        let sp = opSpan <> span e
         in ENeg sp e
    operatorParser = infixOp <|> infixFunction'
    negateOpParser = fst <$> withSpan (hidden $ char '-')
    infixOp = uncurry EVar <$> withSpan (Id <$> opIdentifier)
    infixFunction' = infixFunction EVar ECon

term :: Parser Expr'
term = lexeme term' <?> "expression" where
  term' =  litParser
       <|> withLineFold ifParser
       <|> caseParser
       <|> lamParser
       <|> letParser
       <|> try applyFuncParser
       <|> varParser
       <|> conParser
       <|> parens parser

litParser :: Parser Expr'
litParser = uncurry ELit <$> withSpan Lit.parser

applyFuncParser :: Parser Expr'
applyFuncParser = sameLine $ do
  startPos <- getOffset
  funcName <- lexeme funcNameParser
  -- NOTE: next line is to prevent wrong order of parentheses in nested applications
  args <- some $ lexeme arg
  let sp = span $ fromJust $ nonEmpty args
  pure $ EApp (startPos .> sp) funcName args
  where
    variable = uncurry EVar <$> withSpan (Id <$> identifier)
    constructor = uncurry ECon <$> withSpan (Id <$> capitalIdentifier)
    prefixOp = uncurry EVar <$> withSpan prefixOperator
    funcNameParser =  variable
                  <|> constructor
                  <|> try prefixOp
                  <|> parens parser
    arg =  litParser
       <|> varParser
       <|> conParser
       <|> parens parser

varParser :: Parser Expr'
varParser = uncurry EVar <$> varParser' where
  varParser' = withSpan $ Id <$> (try opVar <|> identifier)
  opVar = betweenParens opIdentifier

conParser :: Parser Expr'
conParser =
  uncurry ECon <$> withSpan (Id <$> capitalIdentifier)

-- NOTE: No linefold added here for body, linefold is already
--       present on binding level
lamParser :: Parser Expr'
lamParser = do
  startPos <- getOffset
  vars <- lexeme' lambdaHead
  body <- parser
  pure $ ELam (startPos .> span body) vars body
  where
    lambdaHead = sameLine $ do
      void . lexeme $ char '\\'
      vars <- some $ lexeme Pattern.parser
      void $ lexeme (chunk "->" <?> "lambda arrow")
      pure vars

ifParser :: Parser Expr'
ifParser = do
  startPos <- getOffset
  keyword "if"
  cond <- lexeme' parser
  keyword "then"
  trueClause <- lexeme' parser
  keyword "else"
  falseClause <- parser
  pure $ EIf (startPos .> span falseClause) cond trueClause falseClause

-- NOTE: Only alignment of clauses is checked here,
--       linefold is already present on binding level
caseParser :: Parser Expr'
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
  pure $ ECase (startPos .> sp) expr' clauses
  where
    clauseParser = withLineFold $ do
      pat <- lexeme' Pattern.parser
      void . lexeme' $ chunk "->"
      expr' <- parser
      pure (pat, expr')

letParser :: Parser Expr'
letParser = do
  (startPos, bindings) <- withLineFold $ do
    startPos <- getOffset
    keyword "let"
    indentation <- indentLevel
    let declParser' = withIndent indentation declParser <?> "declaration"
    decls <- lexeme declParser' `sepBy1` whitespace'
    pure (startPos, decls)
  result <- withLineFold $ (keyword "in" <?> inLabel) *> parser
  pure $ ELet (startPos .> span result) bindings result
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
      body = ELam (startPos .> sp) vars expr'
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

parens :: Parser Expr' -> Parser Expr'
parens p =
  uncurry EParens <$> withSpan (betweenParens p)

