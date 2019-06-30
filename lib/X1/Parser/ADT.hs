
module X1.Parser.ADT ( parser ) where

import Protolude hiding ( Type )
import Data.Maybe ( fromJust )
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span
import X1.Types.Expr1.ADT
import X1.Types.Expr1.Type
import qualified X1.Parser.Type as Type
import qualified X1.Parser.Tycon as Tycon
import qualified X1.Parser.Tyvar as Tyvar
import X1.Parser.Helpers


-- TODO: can be simplified once tycon/tyvar have span info?

parser :: Parser (ADT 'Parsed)
parser = do
  startPos <- getOffset
  keyword "data"
  (sp1, adtHead) <- adtHeadParser <?> "name of datatype"
  (sp2, adtBody) <- withDefault (sp1, []) $ assignChar *> adtBodyParser
  pure $ ADT (startPos .> sp1 <> sp2) adtHead adtBody
  where assignChar = lexeme $ indented $ char '='

adtHeadParser :: Parser (Span, ADTHead)
adtHeadParser = do
  (sp1, name) <- lexeme $ withSpan $ indented Tycon.parser
  vars <- many $ lexeme $ withSpan $ indented Tyvar.parser
  let (spans, vars') = unzip vars
      sp = sconcat $ fromJust $ nonEmpty $ sp1:spans
  pure (sp, ADTHead name vars')

adtBodyParser :: Parser (Span, ADTBody)
adtBodyParser = do
  conDecls <- conDeclParser `sepBy1` pipeChar
  let (spans, body) = unzip conDecls
      sp = sconcat $ fromJust $ nonEmpty spans
  pure (sp, body)
  where pipeChar = lexeme $ indented $ char '|'

conDeclParser :: Parser (Span, ConDecl)
conDeclParser = do
  (sp1, constrName) <- lexeme (withSpan $ Id <$> indented capitalIdentifier) <?> "constructor"
  types <- many (lexeme (withSpan $ indented adtTypeParser) <?> "type")
  let (spans, types') = unzip types
      sp = sconcat $ fromJust $ nonEmpty $ sp1:spans
  pure (sp, ConDecl constrName types')

adtTypeParser :: Parser Type
adtTypeParser =  con
             <|> var
             <|> betweenParens Type.parser
  where con = TCon <$> Tycon.parser
        var = TVar <$> Tyvar.parser

