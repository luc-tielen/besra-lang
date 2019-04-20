module X1.Parser.Pred ( parser ) where

import Protolude hiding ( Type )
import X1.Parser.Helpers
import qualified X1.Parser.Tyvar as Tyvar
import X1.Parser.Types.Pred
import X1.Parser.Types.Type
import X1.Types.Id
import qualified Text.Megaparsec.Char.Lexer as L

parser :: Parser () -> Parser Pred
parser ws' =
  let lexeme' = L.lexeme ws'
   in IsIn <$> lexeme' (Id <$> capitalIdentifier <?> "typeclass identifier")
           <*> some (TVar <$> lexeme' Tyvar.parser)

