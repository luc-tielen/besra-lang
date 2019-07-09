
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Tyvar ( Tyvar(..) ) where

import Protolude
import X1.Types.Id
import X1.Types.Ann


data Tyvar (ph :: Phase) = Tyvar (Ann ph) Id

deriving instance Eq (Ann ph) => Eq (Tyvar ph)
deriving instance Show (Ann ph) => Show (Tyvar ph)
