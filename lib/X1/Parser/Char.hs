
module X1.Parser.Char ( parser ) where

import Protolude
import X1.Parser.Helpers


parser :: Parser Char
parser =
  let character = satisfy (/= '\'')
      singleQuote' = singleQuote <?> "single quote (')"
   in between singleQuote' singleQuote' character

