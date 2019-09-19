
module Besra.Parser.Lit ( parser ) where

import Protolude
import Besra.Types.IR1 ( Lit(..) )
import Besra.Parser.Helpers ( Parser )
import qualified Besra.Parser.Char as Char
import qualified Besra.Parser.String as String
import qualified Besra.Parser.Number as Number

parser :: Parser Lit
parser =
  let
    number = LNumber <$> Number.parser
    string = LString <$> String.parser
    char = LChar <$> Char.parser
  in
    number <|> string <|> char
