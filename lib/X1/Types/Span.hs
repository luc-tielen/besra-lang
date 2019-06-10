
module X1.Types.Span ( Span(..) ) where

import Protolude


data Span = Span { beginPos :: Int, endPos :: Int }
  deriving (Eq, Show)
