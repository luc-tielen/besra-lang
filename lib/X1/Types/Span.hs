
module X1.Types.Span ( Span(..) ) where

import Protolude


data Span = Span { beginPos :: Int, endPos :: Int }
  deriving (Eq, Show)

instance Semigroup Span where
  (Span begin1 end1) <> (Span begin2 end2) =
    Span (min begin1 begin2) (max end1 end2)
