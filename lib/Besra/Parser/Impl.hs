
module Besra.Parser.Impl ( parser ) where

import Protolude hiding ( Type, try, functionName )
import Data.Maybe ( fromJust )
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.IR1.Expr
import Besra.Types.IR1.Type
import Besra.Types.IR1.Pred
import Besra.Types.IR1.Impl
import Besra.Parser.Helpers
import qualified Besra.Parser.Scheme as Scheme
import qualified Besra.Parser.Pattern as Pattern
import qualified Besra.Parser.Expr as Expr
import qualified Besra.Parser.Tycon as Tycon
import qualified Besra.Parser.Tyvar as Tyvar


type Impl' = Impl Parsed
type Binding' = Binding Parsed
type Pred' = Pred Parsed

parser :: Parser Impl'
parser = parser' <?> "impl declaration" where
  parser' = withLineFold $ do
    startPos <- getOffset
    keyword "impl"
    predicates <- Scheme.predicatesPrefix
    typeInfo <- lexeme' implParser
    posBeforeWhere <- getOffset
    kwResult <- keyword' "where"
    let sp1 = Span startPos (posBeforeWhere + 5)
    case kwResult of
      NoTrailingWS -> pure $ Impl sp1 predicates typeInfo []
      TrailingWS -> do
        indent <- indentLevel
        let bindingParser' = withIndent indent (withLineFold bindingParser)
        bindings <- many bindingParser'
        notFollowedBy badlyIndentedDecl <?> badIndentMsg
        let spans = sp1 :| map span bindings
        pure $ Impl (span spans) predicates typeInfo bindings
  badlyIndentedDecl = indented bindingParser
  badIndentMsg = "properly indented binding declaration in impl"

implParser :: Parser Pred'
implParser = do
  startPos <- getOffset
  name <- traitId
  ts <- some (implTypeParser <?> "type")
  let sp = span $ fromJust $ nonEmpty ts
  pure $ IsIn (startPos .> sp) name ts
  where
    traitId = Id <$> lexeme' capitalIdentifier <?> "trait identifier"
    implTypeParser = lexeme' (betweenParens implTypeParser) <|> implType
    implType = computeType <$> tycon <*> many tyvar
    tycon = lexeme' $ TCon <$> Tycon.parser
    tyvar = lexeme' $ do
      notFollowedBy $ chunk "where"
      TVar <$> Tyvar.parser
    computeType con [] = con
    computeType con vars = TApp con vars


-- TODO remove duplication with expr1 parser once type decls are supported in instances
bindingParser :: Parser Binding'
bindingParser = try namedFunctionDecl <|> simpleBinding

namedFunctionDecl :: Parser Binding'
namedFunctionDecl = do
  startPos <- getOffset
  (funcName, vars) <- lexeme' functionHead
  expr <- Expr.parser
  let sp = startPos .> span expr
      body = ELam sp vars expr
  pure $ Binding sp funcName body
  where
    functionHead = sameLine $ do
      funcName <- lexeme declIdentifier
      vars <- some $ lexeme Pattern.parser
      void $ lexeme assign
      pure (funcName, vars)

simpleBinding :: Parser Binding'
simpleBinding = do
  startPos <- getOffset
  var <- lexeme' declIdentifier
  void $ lexeme' assign
  expr <- Expr.parser
  pure $ Binding (startPos .> span expr) var expr

declIdentifier :: Parser Id
declIdentifier = Id <$> identifier <|> prefixOperator

prefixOperator :: Parser Id
prefixOperator = Id <$> sameLine (betweenParens opIdentifier) <?> "operator"

assign :: Parser Char
assign = char '=' <?> "rest of assignment"
