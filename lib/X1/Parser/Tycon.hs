module X1.Parser.Tycon ( parser ) where

import Protolude
import X1.Parser.Helpers
import X1.Types.Expr1.Tycon
import X1.Types.Id
import X1.Types.Ann


parser :: Parser (Tycon 'Parsed)
parser = uncurry Tycon <$> withSpan con
  where con = Id <$> capitalIdentifier <?> "concrete type"

