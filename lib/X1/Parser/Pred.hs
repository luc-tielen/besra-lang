module X1.Parser.Pred ( parser ) where

import Protolude hiding ( Type )
import X1.Parser.Helpers
import qualified X1.Parser.Tyvar as Tyvar
import X1.Types.Expr1.Pred
import X1.Types.Expr1.Type
import X1.Types.Id
import X1.Types.Ann


type Pred' = Pred 'Parsed

parser :: Parser Pred'
parser = IsIn <$> lexeme' (Id <$> capitalIdentifier <?> "typeclass identifier")
              <*> some tyvar
  where
    tyvar = do
      notFollowedBy $ chunk "where"
      TVar <$> lexeme' Tyvar.parser

