
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR2.ADT
  ( ConDecl(..)
  , ADTHead(..)
  , ADTBody
  , ADT(..)
  , adtName
  , adtRefersTo
  ) where

import Protolude hiding ( Type )
import Unsafe ( unsafeHead )
import Besra.Types.Ann
import Besra.Types.Id
import Besra.Types.IR2.Type


data ConDecl (ph :: Phase)
  = ConDecl (Ann ph) Id (Type ph)

data ADTHead ph = ADTHead Id (Type ph)

type ADTBody ph = [ConDecl ph]

data ADT (ph :: Phase) = ADT (Ann ph) (ADTHead ph) (ADTBody ph)

adtName :: ADT ph -> Id
adtName (ADT _ (ADTHead name _) _) = name

adtRefersTo :: ADT ph -> [Id]
adtRefersTo adt@(ADT _ _ conDecls) =
  uniq $ filter (/= name) $ concatMap getRefs conDecls
  where
    name = adtName adt
    uniq = map unsafeHead . group . sort
    getRefs (ConDecl _ _ ty) = getRefsTy ty
    getRefsTy = \case
      TCon (Tycon _ con) -> [con]
      TVar _ -> mempty
      TApp t1 t2 -> getRefsTy t1 <> getRefsTy t2

deriving instance AnnHas Eq ph => Eq (ConDecl ph)
deriving instance AnnHas Eq ph => Eq (ADTHead ph)
deriving instance AnnHas Eq ph => Eq (ADT ph)
deriving instance AnnHas Show ph => Show (ConDecl ph)
deriving instance AnnHas Show ph => Show (ADTHead ph)
deriving instance AnnHas Show ph => Show (ADT ph)

