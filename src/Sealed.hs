module Sealed where

import BatchClass qualified as BC

class Sorted s where
    pull :: (Ord a, BC.Batch b) => s b a -> Maybe (b a, s b a)
