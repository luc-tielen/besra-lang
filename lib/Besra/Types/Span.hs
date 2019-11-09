
module Besra.Types.Span ( Span(..), beginPos, endPos, HasSpan(..), (.>) ) where

import Protolude


data Span = Span {-# UNPACK #-} !Int !Int
  deriving (Eq, Ord, Show)

beginPos :: Span -> Int
beginPos (Span begin _) = begin

endPos :: Span -> Int
endPos (Span _ end) = end

(.>) :: Int -> Span -> Span
(.>) begin endSpan = Span begin begin <> endSpan


instance Semigroup Span where
  (Span begin1 end1) <> (Span begin2 end2) =
    Span (min begin1 begin2) (max end1 end2)

class HasSpan a where
  span :: a -> Span

instance HasSpan Span where
  span = identity

instance HasSpan a => HasSpan (NonEmpty a) where
  span = sconcat . map span

instance HasSpan a => HasSpan (a, b) where
  span (a, _) = span a
