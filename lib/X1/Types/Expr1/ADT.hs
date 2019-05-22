
module X1.Types.Expr1.ADT ( ConDecl(..)
                          , ADTHead(..)
                          , ADTBody
                          , ADT(..)
                          ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Type
import X1.Types.Id


data ConDecl = ConDecl Id [Type]
  deriving (Eq, Show)

data ADTHead = ADTHead Tycon [Tyvar]
  deriving (Eq, Show)

type ADTBody = [ConDecl]

data ADT = ADT ADTHead ADTBody
  deriving (Eq, Show)

