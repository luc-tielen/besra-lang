
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.TypeAnn ( TypeAnn(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Scheme
import X1.Types.Id
import X1.Types.Ann


data TypeAnn (ph :: Phase)
  = TypeAnn Id (Scheme ph)

deriving instance Eq (Ann ph) => Eq (TypeAnn ph)
deriving instance Show (Ann ph) => Show (TypeAnn ph)
