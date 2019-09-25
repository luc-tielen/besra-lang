
module Besra.TypeSystem.FreeTypeVars
  ( FreeTypeVars(..)
  ) where

import Protolude hiding ( Type )
import Besra.Types.IR3 ( Scheme(..), Qual(..), Pred(..), Type(..) )
import Besra.Types.Tyvar
import Besra.Types.Ann
import Data.List (nub, union)


type KI = KindInferred

class FreeTypeVars a where
  ftv :: a -> [Tyvar KI]

instance FreeTypeVars (Type KI) where
  ftv (TVar u) = [u]
  ftv (TApp l r) = ftv l `union` ftv r
  ftv _ = []

instance FreeTypeVars a => FreeTypeVars [a] where
  ftv = nub . concatMap ftv

instance FreeTypeVars (t KI) => FreeTypeVars (Qual KI t) where
  ftv (ps :=> t) = ftv ps `union` ftv t

instance FreeTypeVars (Pred KI) where
  ftv (IsIn _ _ ts) = ftv ts

instance FreeTypeVars (Scheme KI) where
  ftv (ForAll _ _ qt) = ftv qt

