
module X1.Parser.Lit ( parser ) where

import Protolude
import X1.Types.IR1.Lit
import X1.Parser.Helpers ( Parser )
import qualified X1.Parser.Char as Char
import qualified X1.Parser.String as String
import qualified X1.Parser.Number as Number

parser :: Parser Lit
parser =
  let
    number = LNumber <$> Number.parser
    string = LString <$> String.parser
    char = LChar <$> Char.parser
  in
    number <|> string <|> char
