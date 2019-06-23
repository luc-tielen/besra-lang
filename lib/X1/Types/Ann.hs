
module X1.Types.Ann ( Phase(..), Anno, Tag(..), Ann(..) ) where

import Protolude hiding ( show )
import Prelude ( Show(..) )
import X1.Types.Span


-- | Enumeration of the various phases in the compiler.
--   Used by the type families below to provide extra annotations to each node
--   in the AST in a flexible way (based on the "trees that grow" paper).

data Phase = Parsed  -- ^ Phase right after AST is parsed
           | Testing -- ^ Phase only used in the testsuite (for stripping out annotations)


type family Anno (a :: Phase)

type instance Anno 'Parsed = Span
type instance Anno 'Testing = ()

data Tag (a :: Phase) where
  TagP :: Tag 'Parsed
  TagT :: Tag 'Testing

data Ann where
  Ann :: Tag a -> Anno a -> Ann

instance Eq Ann where
  (Ann TagP ann1) == (Ann TagP ann2) = ann1 == ann2
  (Ann TagT ann1) == (Ann TagT ann2) = ann1 == ann2
  _ == _ = False

instance Show Ann where
  show (Ann TagP ann1) = "(" ++ show ann1 ++ ")"
  show (Ann TagT ann1) = show ann1

