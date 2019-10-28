
module Besra.TypeSystem.FreeTypeVars
  ( FreeTypeVars(..)
  ) where

import Protolude hiding ( Type )
import Besra.Types.IR3 ( Scheme(..), Qual(..), Pred(..), Type(..), sameTyvar )
import Besra.Types.Tyvar
import Besra.Types.Ann
import Data.List (nubBy, unionBy)


type KI = KindInferred

class FreeTypeVars a where
  ftv :: a -> [Tyvar KI]

instance FreeTypeVars (Type KI) where
  ftv (TVar u) = [u]
  ftv (TApp l r) =
    let vs1 = ftv l
        vs2 = ftv r
     in unionBy sameTyvar vs1 vs2
  ftv _ = []

instance FreeTypeVars a => FreeTypeVars [a] where
  ftv = nubBy sameTyvar . concatMap ftv

instance FreeTypeVars (t KI) => FreeTypeVars (Qual KI t) where
  ftv (ps :=> t) =
    let vs1 = ftv ps
        vs2 = ftv t
     in unionBy sameTyvar vs1 vs2

instance FreeTypeVars (Pred KI) where
  ftv (IsIn _ _ ts) = ftv ts

instance FreeTypeVars (Scheme KI) where
  ftv (ForAll _ _ qt) = ftv qt

