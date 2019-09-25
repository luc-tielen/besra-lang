
module Besra.TypeSystem.Error
  ( Error(..)
  ) where

import Protolude hiding ( Type )
import Besra.Types.IR3 ( Tyvar, Type, Pred, Scheme, Explicit )
import Besra.Types.Kind
import Besra.Types.Span
import Besra.Types.Ann
import Besra.Types.Id


type KI = KindInferred
type Substitution = [(Tyvar KI, Type KI)]

data Error
  = MergeFail Substitution Substitution
  | UnificationFailure (Type KI) (Type KI)
  | ListUnificationFailure Int Int
  | OccursCheck (Tyvar KI) (Type KI)
  | TraitMismatch (Pred KI) (Pred KI)
  | KindMismatch (Tyvar KI) (Type KI) Kind Kind
  | TypeMismatch (Type KI) (Type KI)
  | UnboundIdentifier Span Id
  | UnknownTrait Span Id
  | NoTraitForImpl Span Id
  | NoImplsForTrait (Pred KI)
  | OverlappingImpls (Pred KI) [Pred KI]
  | TraitAlreadyDefined Span Span Id
  | SuperTraitNotDefined (Pred KI)
  | TooGeneralSignatureGiven (Scheme KI) (Scheme KI)
  | ContextTooWeak (Explicit KI) [Pred KI]
  | AmbiguousDefaults [Tyvar KI] [Pred KI]
  deriving (Eq, Show)

