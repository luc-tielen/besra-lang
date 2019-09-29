module Besra.TypeSystem.Instantiate
  ( Instantiate(..)
  ) where

import Protolude hiding (Type)
import Besra.Types.IR3 (Type(..), Pred(..), Qual(..))
import Besra.Types.Ann
import Data.List ((!!))


class Instantiate t where
  inst :: [Type PreTC] -> t -> t

instance Instantiate (Type PreTC) where
  inst ts (TApp l r) = TApp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst _ t          = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate (t PreTC) => Instantiate (Qual PreTC t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate (Pred PreTC) where
  inst ts (IsIn ann c t) = IsIn ann c (inst ts t)

