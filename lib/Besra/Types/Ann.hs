
module Besra.Types.Ann
  ( Phase
  , Parsed
  , KindInferred
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
data Phase
  = Parsed        -- ^ Phase right after AST is parsed
  | KindInferred  -- ^ Phase right after kind inference
  | PostTC        -- ^ Phase right after typechecking
  | Testing       -- ^ Phase only used in the testsuite (for stripping out annotations)

type Parsed = 'Parsed
type KindInferred = 'KindInferred
type PostTC = 'PostTC
type Testing = 'Testing


type family Ann (a :: Phase) where
  Ann Parsed = Span
  Ann KindInferred = Span
  Ann PostTC = Span
  Ann Testing = ()

type family AnnTy (a :: Phase) where
  AnnTy Parsed = Span
  AnnTy KindInferred = (Span, Kind)
  AnnTy PostTC = (Span, Kind)
  AnnTy Testing = ()

type AnnHas (f :: Type -> Constraint) (ph :: Phase)
  = (f (Ann ph), f (AnnTy ph))

