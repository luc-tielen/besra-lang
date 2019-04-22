module X1.Parser.Tyvar ( parser ) where

import Protolude
import X1.Parser.Helpers
import X1.Parser.Types.Tyvar
import X1.Types.Id


parser :: Parser Tyvar
parser = Tyvar . Id <$> identifier <?> "type variable"

