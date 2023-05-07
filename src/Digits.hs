module Digits where

import BatchClass qualified as BC
import Sealed qualified as S


-- Keep everything in meldable order for now.
-- Worry about findable order afterwards.
type Digits s b a = [s b a]

-- We can go up to double capacity, because of how we handle carries.
meld :: (Ord a, S.Sorted s b) => Int -> [s b a] -> [s b a] -> [s b a]
meld cap a b = normalise $ add a b  where
    add [] ys = ys
    add xs [] = xs
    add (x:xs) (y:ys) = x <> y : add xs ys

    normalise [] = []
    normalise (x:xs)
        | S.len x > cap = mempty : normalise (meld' x xs)
        | otherwise = x : normalise xs
    meld' x = case S.meld x of
        Nothing -> id
        Just x' -> add [S.insert x' mempty]
