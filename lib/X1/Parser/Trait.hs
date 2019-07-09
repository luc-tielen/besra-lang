
module X1.Parser.Trait ( parser ) where

import Protolude
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Expr1.TypeAnn
import X1.Types.Expr1.Trait
import X1.Parser.Helpers
import qualified X1.Parser.Pred as Pred
import qualified X1.Parser.Scheme as Scheme


type Trait' = Trait 'Parsed
type TypeAnn' = TypeAnn 'Parsed

parser :: Parser Trait'
parser = parser' <?> "trait declaration" where
  parser' = withLineFold $ do
    keyword "trait"
    predicates <- Scheme.predicatesPrefix
    trait <- lexeme' Pred.parser
    kwResult <- keyword' "where"
    case kwResult of
      NoTrailingWS -> pure $ Trait predicates trait []
      TrailingWS -> do
        indent <- indentLevel
        let typeAnnParser' = withIndent indent (withLineFold typeAnnParser)
        typeAnns <- many typeAnnParser'
        notFollowedBy badlyIndentedDecl <?> badIndentMsg
        pure $ Trait predicates trait typeAnns
  badlyIndentedDecl = void (indented typeAnnParser) <|> void (char ':')
  badIndentMsg = "properly indented type declaration in trait"

-- TODO remove duplication with expr1 parser once binding decls are supported in traits
typeAnnParser :: Parser TypeAnn'
typeAnnParser = do
  var <- lexeme' declIdentifier
  void $ lexeme' typeSeparator
  TypeAnn var <$> Scheme.parser
  where
    declIdentifier = declVar <|> prefixOperator
    declVar = Id <$> identifier <?> "variable"
    typeSeparator = char ':' <?> "rest of type declaration"
    prefixOperator = Id <$> sameLine (betweenParens opIdentifier) <?> "operator"

