module Sealed where

import Data.List qualified as DL

import BatchClass qualified as BC
import Data.Function (on)
-- import Data.Ord (compare)

class BC.Batch b => Sorted s b where
    -- double pull
    pull2 :: Ord a => s b a -> Maybe (Heap s b a)
    pull2 s = case pull1 s of
        Nothing -> Nothing
        Just (Heap b1 s1) -> case pull1 s1 of
            Nothing -> Just (Heap b1 s1)
            Just (Heap b2 s2) -> Just (Heap (b1 <> b2) s2)

    pull1 :: Ord a => s b a -> Maybe (Heap s b a)
    insert :: Ord a => Heap s b a -> s b a -> s b a

data Heap s b a = Heap { batch :: b a, rest :: s b a }
newtype SList b a = SList [Heap SList b a]

instance BC.Batch b => Sorted SList b where
    insert x (SList xs) = SList $ DL.insertBy (compare `on` BC.key . batch) x xs
    pull1 (SList []) = Nothing
    pull1 (SList (Heap batch0 rest:xs))
        = Just (Heap batch0 $ maybe id insert (pull2 rest) (SList xs))

-- pop :: 
