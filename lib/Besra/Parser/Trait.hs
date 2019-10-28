
module Besra.Parser.Trait ( parser ) where

import Protolude
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span
import Besra.Types.IR1 ( TypeAnn(..), Trait(..) )
import Besra.Parser.Helpers
import qualified Besra.Parser.Pred as Pred
import qualified Besra.Parser.Scheme as Scheme


type Trait' = Trait Parsed
type TypeAnn' = TypeAnn Parsed

parser :: Parser Trait'
parser = parser' <?> "trait declaration" where
  parser' = withLineFold $ do
    startPos <- getOffset
    keyword "trait"
    predicates <- Scheme.predicatesPrefix
    trait <- lexeme' Pred.parser
    posBeforeWhere <- getOffset
    kwResult <- keyword' "where"
    let sp = Span startPos (posBeforeWhere + 5)
    case kwResult of
      NoTrailingWS -> pure $ Trait sp predicates trait []
      TrailingWS -> do
        indent <- indentLevel
        let typeAnnParser' = withIndent indent (withLineFold typeAnnParser)
        typeAnns <- many typeAnnParser'
        notFollowedBy badlyIndentedDecl <?> badIndentMsg
        let sp' = maybe sp span (nonEmpty typeAnns)
        pure $ Trait (startPos .> sp') predicates trait typeAnns
  badlyIndentedDecl = void (indented typeAnnParser) <|> void (char ':')
  badIndentMsg = "properly indented type declaration in trait"

-- TODO remove duplication with expr1 parser once binding decls are supported in traits
typeAnnParser :: Parser TypeAnn'
typeAnnParser = do
  startPos <- getOffset
  var <- lexeme' declIdentifier
  void $ lexeme' typeSeparator
  scheme <- Scheme.parser
  pure $ TypeAnn (startPos .> span scheme) var scheme
  where
    declIdentifier = declVar <|> prefixOperator
    declVar = Id <$> identifier <?> "variable"
    typeSeparator = char ':' <?> "rest of type declaration"
    prefixOperator = Id <$> sameLine (betweenParens opIdentifier) <?> "operator"

