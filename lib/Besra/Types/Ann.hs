
module Besra.Types.Ann
  ( Phase
  , PhaseTag(..)
  , ToTag(..)
  , Parsed
  , KindInferred
  , PreTC
  , PostTC
  , Testing
  , Ann
  , AnnTy
  , AnnHas
  ) where

import Protolude
import Besra.Types.Span
import Besra.Types.Kind


-- | Enumeration of the various phases in the compiler.
--   Used by the type family below to provide extra annotations to each node
--   in the AST in a flexible way (based on the "trees that grow" paper).
data Phase = Parsed        -- ^ Phase right after AST is parsed
           | KindInferred  -- ^ Phase right after kind inference
           | PreTC         -- ^ Phase right before typechecking
           | PostTC        -- ^ Phase right after typechecking
           | Testing       -- ^ Phase only used in the testsuite (for stripping out annotations)

type Parsed = 'Parsed
type KindInferred = 'KindInferred
type PreTC = 'PreTC
type PostTC = 'PostTC
type Testing = 'Testing

-- TODO remove this type when no longer needed
-- (after bidirectional typechecking or when spans are not taken into account anymore)
data PhaseTag (ph :: Phase) where
  TagP :: PhaseTag Parsed
  TagKI :: PhaseTag KindInferred
  TagPreTC :: PhaseTag PreTC
  TagPostTC :: PhaseTag PostTC
  TagT :: PhaseTag Testing

class ToTag (ph :: Phase) where
  toTag :: PhaseTag ph

instance ToTag Parsed where
  toTag = TagP

instance ToTag KindInferred where
  toTag = TagKI

instance ToTag PreTC where
  toTag = TagPreTC

instance ToTag PostTC where
  toTag = TagPostTC

instance ToTag Testing where
  toTag = TagT

type family Ann (a :: Phase) where
  Ann Parsed = Span
  Ann KindInferred = Span
  Ann PreTC = Span
  Ann PostTC = Span
  Ann Testing = ()

type family AnnTy (a :: Phase) where
  AnnTy Parsed = Span
  AnnTy KindInferred = (Span, Kind)
  AnnTy PreTC = (Span, Kind)
  AnnTy PostTC = (Span, Kind)
  AnnTy Testing = ()

type AnnHas (f :: Type -> Constraint) (ph :: Phase)
  = (ToTag ph, f (Ann ph), f (AnnTy ph))

