
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Tycon ( Tycon(..) ) where

import Protolude
import X1.Types.Id
import X1.Types.Ann


data Tycon (ph :: Phase)
  = Tycon (Ann ph) Id

deriving instance Eq (Ann ph) => Eq (Tycon ph)
deriving instance Show (Ann ph) => Show (Tycon ph)

