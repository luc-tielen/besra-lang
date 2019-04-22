
module X1.Parser.Char ( parser ) where

import Protolude
import X1.Parser.Helpers


parser :: Parser Char
parser =
  let character = satisfy (/= '\'') <?> "character literal"
      openSingleQuote = singleQuote <?> "character literal"
      closeSingleQuote = singleQuote <?> "closing single quote (')"
   in between openSingleQuote closeSingleQuote character

