
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.Module ( Module(..) ) where

import Protolude
import Besra.Types.IR2.Expr ( Decl )
import Besra.Types.Ann


newtype Module (ph :: Phase)
  = Module [Decl ph]

deriving instance Eq (Ann ph) => Eq (Module ph)
deriving instance Show (Ann ph) => Show (Module ph)

