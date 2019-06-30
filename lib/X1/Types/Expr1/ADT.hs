
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.ADT ( ConDecl(..)
                          , ADTHead(..)
                          , ADTBody
                          , ADT(..)
                          ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Type
import X1.Types.Id
import X1.Types.Ann


data ConDecl = ConDecl Id [Type]
  deriving (Eq, Show)

data ADTHead = ADTHead Tycon [Tyvar]
  deriving (Eq, Show)

type ADTBody = [ConDecl]

data ADT (ph :: Phase)
  = ADT (Ann ph) ADTHead ADTBody

deriving instance Eq (Ann ph) => Eq (ADT ph)
deriving instance Show (Ann ph) => Show (ADT ph)
