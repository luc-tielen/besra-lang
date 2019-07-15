module X1.Parser.Pred ( parser ) where

import Protolude hiding ( Type )
import Data.Maybe ( fromJust )
import X1.Parser.Helpers
import qualified X1.Parser.Tyvar as Tyvar
import X1.Types.IR1.Pred
import X1.Types.IR1.Type
import X1.Types.Id
import X1.Types.Ann
import X1.Types.Span


type Pred' = Pred 'Parsed

parser :: Parser Pred'
parser = do
  startPos <- getOffset
  name <- lexeme' (Id <$> capitalIdentifier <?> "typeclass identifier")
  vars <- some tyvar
  let sp = span $ fromJust $ nonEmpty vars
  pure $ IsIn (startPos .> sp) name vars
  where
    tyvar = do
      notFollowedBy $ chunk "where"
      TVar <$> lexeme' Tyvar.parser

