
module X1.Parser.String ( parser ) where

import Protolude
import X1.Parser.Types.String
import X1.Parser.Helpers
import Text.Megaparsec (takeWhileP)


parser :: Parser String
parser =
  let quote = char '"'
      stringChars = takeWhileP Nothing (/= '"')
   in String <$> between quote quote stringChars <?> "string"

