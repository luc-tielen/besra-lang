
module X1.Parser.Types.Type ( Type(..), Tycon(..), Tyvar(..) ) where

import Protolude hiding ( Type )
import X1.Parser.Types.Tycon
import X1.Parser.Types.Tyvar


data Type = TCon Tycon
          | TVar Tyvar
          | TApp Type [Type]
          deriving (Eq, Show)

