module Besra.Parser.Tyvar ( parser ) where

import Protolude
import Besra.Parser.Helpers
import Besra.Types.IR1.Tyvar
import Besra.Types.Id
import Besra.Types.Ann


parser :: Parser (Tyvar 'Parsed)
parser = uncurry Tyvar <$> withSpan var
  where
    var = Id <$> identifier <?> "type variable"


