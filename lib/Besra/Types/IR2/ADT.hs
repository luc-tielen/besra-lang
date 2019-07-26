
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.ADT
  ( ConDecl(..)
  , ADTHead(..)
  , ADTBody
  , ADT(..)
  ) where

import Protolude hiding ( Type )
import Besra.Types.Ann
import Besra.Types.Id
import Besra.Types.IR2.Type


data ConDecl (ph :: Phase)
  = ConDecl (Ann ph) Id (Type ph)

data ADTHead ph = ADTHead Id (Type ph)

type ADTBody ph = [ConDecl ph]

data ADT (ph :: Phase) = ADT (Ann ph) (ADTHead ph) (ADTBody ph)

deriving instance Eq (Ann ph) => Eq (ConDecl ph)
deriving instance Eq (Ann ph) => Eq (ADTHead ph)
deriving instance Eq (Ann ph) => Eq (ADT ph)
deriving instance Show (Ann ph) => Show (ConDecl ph)
deriving instance Show (Ann ph) => Show (ADTHead ph)
deriving instance Show (Ann ph) => Show (ADT ph)

