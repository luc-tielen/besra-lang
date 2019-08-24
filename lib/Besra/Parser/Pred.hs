module Besra.Parser.Pred ( parser ) where

import Protolude hiding ( Type )
import Data.Maybe ( fromJust )
import Besra.Parser.Helpers
import qualified Besra.Parser.Tyvar as Tyvar
import Besra.Types.IR1.Pred
import Besra.Types.IR1.Type
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span


type Pred' = Pred Parsed

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

