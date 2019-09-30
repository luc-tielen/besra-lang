
module Besra.TypeSystem.Error
  ( Error(..)
  ) where

import Protolude hiding ( Type )
import Besra.Types.IR3 ( Tyvar, Type, Pred, Scheme, Explicit )
import Besra.Types.Kind
import Besra.Types.Span
import Besra.Types.Ann
import Besra.Types.Id


type Substitution = [(Tyvar PreTC, Type PreTC)]

data Error
  = MergeFail Substitution Substitution
  | UnificationFailure (Type PreTC) (Type PreTC)
  | ListUnificationFailure Int Int
  | OccursCheck (Tyvar PreTC) (Type PreTC)
  | TraitMismatch (Pred PreTC) (Pred PreTC)
  | KindMismatch (Tyvar PreTC) (Type PreTC) Kind Kind
  | TypeMismatch (Type PreTC) (Type PreTC)
  | ExplicitTypeMismatch (Scheme PreTC) (Scheme PreTC)
  | UnboundIdentifier Span Id
  | UnknownTrait Span Id
  | NoTraitForImpl Span Id
  | NoImplsForTrait (Pred PreTC)
  | OverlappingImpls (Pred PreTC) [Pred PreTC]
  | TraitAlreadyDefined Span Span Id
  | SuperTraitNotDefined (Pred PreTC)
  | ContextTooWeak (Explicit PreTC) [Pred PreTC]
  | AmbiguousDefaults [Tyvar PreTC] [Pred PreTC]
  deriving (Eq, Show)

