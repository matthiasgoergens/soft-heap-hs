module Scratch2 where

import Data.Bool (bool)
import Control.Arrow ((>>>), (<<<))
import Data.List qualified as DL
import Data.List.NonEmpty qualified as N
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid.Instances.Concat
import Data.Monoid.Instances.Concat ( force )
import Data.Monoid.Factorial ( splitPrimePrefix )
import Data.Monoid.Inf (Inf (..), Pos)
import Data.Maybe (listToMaybe)
import Data.Functor
import Data.Monoid.Null
import Prelude hiding (null, even)
import Data.Coerce
import Data.Function (on)
import Data.Monoid
import Data.Semigroup hiding (Last)
import Data.Monoid.Null

import Batch qualified as B
import Batch (Batch)

data Unsealed
data Sealed

class Nursery n where
    new :: Int -> n Unsealed a
    insert :: Ord a => a -> n Unsealed a -> (n Unsealed a, n Sealed a)

-- data Pennant a =

-- class SealedHeap n where

-- Need to detect empty?
data SealedHeap a = Node (Batch a) (Closed (SealedHeap a))

-- instance Ord a => Semigroup (Batch a) where
--     Batch a <> Batch b = 
--         | x <= 0 = Batch y b
--         -- With careful usage, we could remove the comparison here.
--         | otherwise = Batch (x + y) (max a b)
-- instance Ord a => Monoid (Batch a) where
--     mempty = 


-- type Open a = (Int, [a])
type Closed a = [a]

-- pop1 :: SealedHap a -> 

pullFromHeap :: SealedHeap a -> (Batch a, SealedHeap a)
pullFromHeap (Node b rest) = (b, uncurry Node $ pull2Batch rest)

pull2Batch :: Closed (SealedHeap a) -> (Batch a, Closed (SealedHeap a))
pull2Batch [] = (mempty, [])
-- pull2Batch (x:xs) =
--     let (b0, xs') = pull1Batch x
--         (b1, x'') = pullFromHeap 

pull1Batch :: Closed (SealedHeap a) -> (Batch a, Closed (SealedHeap a))
pull1Batch = undefined
