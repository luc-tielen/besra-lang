
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


data ConDecl (ph :: Phase)
  = ConDecl Id [Type ph]

data ADTHead ph = ADTHead (Tycon ph) [Tyvar ph]

type ADTBody ph = [ConDecl ph]

data ADT (ph :: Phase)
  = ADT (Ann ph) (ADTHead ph) (ADTBody ph)

deriving instance Eq (Ann ph) => Eq (ConDecl ph)
deriving instance Show (Ann ph) => Show (ConDecl ph)
deriving instance Eq (Ann ph) => Eq (ADTHead ph)
deriving instance Show (Ann ph) => Show (ADTHead ph)
deriving instance Eq (Ann ph) => Eq (ADT ph)
deriving instance Show (Ann ph) => Show (ADT ph)
