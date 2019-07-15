module X1.Parser.Tyvar ( parser ) where

import Protolude
import X1.Parser.Helpers
import X1.Types.IR1.Tyvar
import X1.Types.Id
import X1.Types.Ann


parser :: Parser (Tyvar 'Parsed)
parser = uncurry Tyvar <$> withSpan var
  where
    var = Id <$> identifier <?> "type variable"


