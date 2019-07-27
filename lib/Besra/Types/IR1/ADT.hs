
{-# LANGUAGE UndecidableInstances #-}

module Besra.Types.IR1.ADT ( ConDecl(..)
                          , ADTHead(..)
                          , ADTBody
                          , ADT(..)
                          ) where

import Protolude hiding ( Type )
import Besra.Types.IR1.Type
import Besra.Types.Id
import Besra.Types.Ann
import Besra.Types.Span


data ConDecl (ph :: Phase)
  = ConDecl (Ann ph) Id [Type ph]

data ADTHead ph = ADTHead (Tycon ph) [Tyvar ph]

type ADTBody ph = [ConDecl ph]

data ADT (ph :: Phase)
  = ADT (Ann ph) (ADTHead ph) (ADTBody ph)

deriving instance AnnHas Eq ph => Eq (ConDecl ph)
deriving instance AnnHas Eq ph => Eq (ADTHead ph)
deriving instance AnnHas Eq ph => Eq (ADT ph)
deriving instance AnnHas Show ph => Show (ConDecl ph)
deriving instance AnnHas Show ph => Show (ADTHead ph)
deriving instance AnnHas Show ph => Show (ADT ph)

instance AnnHas HasSpan ph => HasSpan (ConDecl ph) where
  span (ConDecl ann _ _) = span ann

instance AnnHas HasSpan ph => HasSpan (ADTHead ph) where
  span (ADTHead name vars) = span $ span name :| map span vars

instance AnnHas HasSpan ph => HasSpan (ADT ph) where
  span (ADT ann _ _) = span ann

