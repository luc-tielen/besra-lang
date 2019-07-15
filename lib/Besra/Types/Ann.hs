
module Besra.Types.Ann ( Phase(..), Ann ) where

import Besra.Types.Span


-- | Enumeration of the various phases in the compiler.
--   Used by the type family below to provide extra annotations to each node
--   in the AST in a flexible way (based on the "trees that grow" paper).
data Phase = Parsed  -- ^ Phase right after AST is parsed
           | Testing -- ^ Phase only used in the testsuite (for stripping out annotations)


type family Ann (a :: Phase) where
  Ann 'Parsed = Span
  Ann 'Testing = ()
