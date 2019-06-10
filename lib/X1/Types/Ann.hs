
module X1.Types.Ann ( Phase(..), Ann ) where

import X1.Types.Span


-- | Enumeration of the various phases in the compiler.
--   Used by the type families below to provide extra annotations to each node
--   in the AST in a flexible way (based on the "trees that grow" paper).

data Phase = Parsed  -- ^ Phase right after AST is parsed
           | Testing -- ^ Phase only used in the testsuite (for stripping out annotations)


type family Ann (a :: Phase)

type instance Ann 'Parsed = Span
type instance Ann 'Testing = ()
