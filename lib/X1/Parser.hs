module X1.Parser ( Parser, parse, parseFile ) where

-- Mostly based on https://www.haskell.org/onlinereport/lexemes.html,
-- with some minor variations.

import Protolude
import X1.Parser.Helpers ( Parser, ParseResult )
import qualified X1.Parser.Module as Module
import X1.Types.Module
import qualified Text.Megaparsec as P


parse :: Parser a -> FilePath -> Text -> ParseResult a
parse p path txt = flip runReader (P.mkPos 1) $ P.runParserT p path txt

parseFile :: FilePath -> Text -> ParseResult (Module Decl)
parseFile = parse Module.parser
