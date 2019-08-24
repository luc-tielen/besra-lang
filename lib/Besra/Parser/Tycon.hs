module Besra.Parser.Tycon ( parser ) where

import Protolude
import Besra.Parser.Helpers
import Besra.Types.Tycon
import Besra.Types.Id
import Besra.Types.Ann


parser :: Parser (Tycon Parsed)
parser = uncurry Tycon <$> withSpan con
  where con = Id <$> capitalIdentifier <?> "concrete type"

