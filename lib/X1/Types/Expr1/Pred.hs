
{-# LANGUAGE UndecidableInstances #-}

module X1.Types.Expr1.Pred ( Pred(..) ) where

import Protolude hiding ( Type )
import X1.Types.Expr1.Type
import X1.Types.Id
import X1.Types.Ann


data Pred (ph :: Phase)
  = IsIn Id [Type ph]

deriving instance Eq (Ann ph) => Eq (Pred ph)
deriving instance Show (Ann ph) => Show (Pred ph)
