module X1.Parser ( Parser, ParseError, parse, parseFile ) where

-- Mostly based on https://www.haskell.org/onlinereport/lexemes.html,
-- with some minor variations.

import Protolude
import X1.Parser.Helpers ( Parser, ParseError, ParseResult, ParseState(..), ParseMode(..) )
import qualified X1.Parser.Module as Module
import X1.Types.Expr1.Module
import qualified Text.Megaparsec as P


parse :: Parser a -> FilePath -> Text -> ParseResult a
parse p path txt =
  let beginState = ParseState P.pos1 Normal
   in flip runReader beginState $ P.runParserT p path txt

parseFile :: FilePath -> Text -> ParseResult Module
parseFile = parse Module.parser
