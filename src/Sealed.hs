module Sealed where

import Data.List qualified as DL

import BatchClass qualified as BC
import Data.Function (on)
import Control.Monad ((>=>))
-- import Data.Ord (compare)

class (BC.Batch b, forall a . Ord a => Monoid (s b a)) => Sorted s b where
    -- double pull
    pull2 :: Ord a => s b a -> Maybe (Heap s b a)
    pull2 s = case pull1 s of
        Nothing -> Nothing
        Just (Heap b1 s1) -> case pull1 s1 of
            Nothing -> Just (Heap b1 s1)
            Just (Heap b2 s2) -> Just (Heap (b1 <> b2) s2)

    pull1 :: Ord a => s b a -> Maybe (Heap s b a)
    insert :: Ord a => Heap s b a -> s b a -> s b a

    uncons :: s b a -> Maybe (Heap s b a, s b a)

    -- This assumes that our list is already in order.
    pop1 :: Ord a => s b a -> Maybe (s b a)
    pop1 = uncons >=> \(heap, rest) -> do
        heap' <- popHeap heap
        return $ insert heap' rest
    len :: s b a -> Int

data Heap s b a = Heap { batch :: b a, rest :: s b a }
newtype SList b a = SList [Heap SList b a]

-- We could do this one with fewer comparisons.
-- But it would still be O(n) anyway.
instance (BC.Batch b, Ord a) => Semigroup (SList b a) where
    SList a <> SList b = SList $ DL.sortBy (compare `on` BC.key . batch) (a <> b)

instance (BC.Batch b, Ord a) => Monoid (SList b a) where
    mempty = SList mempty

instance BC.Batch b => Sorted SList b where
    insert x (SList xs) = SList $ DL.insertBy (compare `on` BC.key . batch) x xs
    pull1 (SList []) = Nothing
    pull1 (SList (Heap batch0 rest:xs))
        = Just (Heap batch0 $ maybe id insert (pull2 rest) (SList xs))
    uncons (SList []) = Nothing
    uncons (SList (x:xs)) = Just (x, SList xs)
    len (SList xs) = length xs

popHeap :: (Ord a, Sorted s b) => Heap s b a -> Maybe (Heap s b a)
popHeap (Heap b rest) = case BC.pop b of
    Just b' -> Just (Heap b' rest)
    -- See whether we need to do pull2 here for asymptotics?
    Nothing -> popHeap =<< pull1 rest
-- pop (Heap b rest) = case BC.pop

-- For this one, we need access to the first?
-- popSorted :: (Ord a, Sorted s b) => s b a -> s b a
-- popSorted 

meld :: (Ord a, Sorted s b) => s b a -> Maybe (Heap s b a)
meld = pull1

-- meld = uncons >=> \(heap, rest) -> 
