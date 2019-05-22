
module X1.Parser.ADT ( parser ) where

import Protolude hiding ( Type )
import X1.Types.Id
import X1.Types.Expr1.ADT
import X1.Types.Expr1.Type
import qualified X1.Parser.Type as Type
import qualified X1.Parser.Tycon as Tycon
import qualified X1.Parser.Tyvar as Tyvar
import X1.Parser.Helpers


parser :: Parser ADT
parser = withLineFold $ do
  keyword "data"
  adtHead <- adtHeadParser <?> "name of datatype"
  adtBody <- withDefault [] $ lexeme' (char '=') *> adtBodyParser
  pure $ ADT adtHead adtBody

adtHeadParser :: Parser ADTHead
adtHeadParser = do
  name <- lexeme' Tycon.parser
  vars <- many $ lexeme' Tyvar.parser
  pure $ ADTHead name vars

adtBodyParser :: Parser ADTBody
adtBodyParser =
  lexeme' conDeclParser `sepBy1` lexeme' separator
  where
    separator = char '|'

conDeclParser :: Parser ConDecl
conDeclParser = do
  constrName <- Id <$> lexeme' capitalIdentifier <?> "constructor"
  types <- many (lexeme' adtTypeParser <?> "type")
  pure $ ConDecl constrName types

adtTypeParser :: Parser Type
adtTypeParser = con <|> var <|> betweenParens Type.parser
  where
    con = TCon <$> Tycon.parser
    var = TVar <$> Tyvar.parser

