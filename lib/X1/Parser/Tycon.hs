module X1.Parser.Tycon ( parser ) where

import Protolude
import X1.Parser.Helpers
import X1.Parser.Types.Tycon
import X1.Types.Id


parser :: Parser Tycon
parser = Tycon . Id <$> capitalIdentifier <?> "concrete type"

