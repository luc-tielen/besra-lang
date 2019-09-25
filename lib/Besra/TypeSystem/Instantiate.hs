module Besra.TypeSystem.Instantiate
  ( Instantiate(..)
  ) where

import Protolude hiding (Type)
import Besra.Types.IR3 (Type(..), Pred(..), Qual(..))
import Besra.Types.Ann
import Data.List ((!!))


type KI = KindInferred

class Instantiate t where
  inst :: [Type KI] -> t -> t

instance Instantiate (Type KI) where
  inst ts (TApp l r) = TApp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst _ t          = t

instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)

instance Instantiate (t KI) => Instantiate (Qual KI t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate (Pred KI) where
  inst ts (IsIn ann c t) = IsIn ann c (inst ts t)

