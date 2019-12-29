
module Besra.Types.Ann
  ( Phase
  , Parsed
  , KindInferred
  , TC
  , Testing
  , AnnTy
  ) where

import Besra.Types.Span
import Besra.Types.Kind


-- | Enumeration of the various phases in the compiler.
--   Used by the type family below to provide extra annotations to each node
--   in the AST in a flexible way (based on the "trees that grow" paper).
data Phase
  = Parsed        -- ^ Phase right after AST is parsed
  | KindInferred  -- ^ Phase right after kind inference
  | TC            -- ^ Typechecking phase
  | Testing       -- ^ Phase only used in the testsuite (for stripping out annotations)

type Parsed = 'Parsed
type KindInferred = 'KindInferred
type TC = 'TC
type Testing = 'Testing

type family AnnTy (a :: Phase) where
  AnnTy Parsed = Span
  AnnTy KindInferred = (Span, Kind)
  AnnTy TC = (Span, Kind)
  AnnTy Testing = ()

