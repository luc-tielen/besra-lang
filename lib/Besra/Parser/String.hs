
module Besra.Parser.String ( parser ) where

import Protolude
import Besra.Types.IR1 ( String(..) )
import Besra.Parser.Helpers


parser :: Parser String
parser =
  let quote = char '"'
      stringChars = takeWhileP Nothing (/= '"')
   in String <$> between quote quote stringChars <?> "string"

