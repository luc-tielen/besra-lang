
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.Trait
  ( Trait(..)
  , traitName
  , traitRefersTo
  ) where

import Protolude
import Unsafe ( unsafeHead )
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.IR2.Pred
import Besra.Types.IR2.Scheme
import Besra.Types.IR2.Expr ( TypeAnn(..) )


data Trait (ph :: Phase)
  = Trait (Ann ph) [Pred ph] (Pred ph) [TypeAnn ph]

traitName :: Trait ph -> Id
traitName (Trait _ _ (IsIn _ name _) _) = name

traitRefersTo :: Trait ph -> [Id]
traitRefersTo t@(Trait _ ps _ tys) =
  uniq $ filter (/= name) $ map getRefs ps <> concatMap getRefsTy tys
  where
    name = traitName t
    uniq = map unsafeHead . group . sort
    getRefs (IsIn _ id _) = id
    getRefsTy (TypeAnn _ _ (Scheme _ ps' _)) = map getRefs ps'

deriving instance AnnHas Eq ph => Eq (Trait ph)
deriving instance AnnHas Show ph => Show (Trait ph)
