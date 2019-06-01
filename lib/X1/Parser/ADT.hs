
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
parser = do
  keyword "data"
  adtHead <- adtHeadParser <?> "name of datatype"
  adtBody <- withDefault [] $ assignChar *> adtBodyParser
  pure $ ADT adtHead adtBody
  where assignChar = lexeme $ indented $ char '='

adtHeadParser :: Parser ADTHead
adtHeadParser = do
  name <- lexeme $ indented Tycon.parser
  vars <- many $ lexeme $ indented Tyvar.parser
  pure $ ADTHead name vars

adtBodyParser :: Parser ADTBody
adtBodyParser = conDeclParser `sepBy1` pipeChar
  where pipeChar = lexeme $ indented $ char '|'

conDeclParser :: Parser ConDecl
conDeclParser = do
  constrName <- Id <$> lexeme (indented capitalIdentifier) <?> "constructor"
  types <- many (lexeme (indented adtTypeParser) <?> "type")
  pure $ ConDecl constrName types

adtTypeParser :: Parser Type
adtTypeParser =  con
             <|> var
             <|> betweenParens Type.parser
  where con = TCon <$> Tycon.parser
        var = TVar <$> Tyvar.parser

