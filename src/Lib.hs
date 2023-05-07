module Lib where

import Control.Arrow ((>>>))
-- import Data.List.NonEmpty (NonEmpty)

import Data.Function (on)
import Data.List.NonEmpty qualified as N
import Data.Monoid.Inf (Inf (..), Pos)
import Data.Monoid.Instances.Concat
import Data.Monoid.Null (MonoidNull (null))

import Data.List qualified as DL

{-
SOFT HEAPS SIMPLIFIED
https://sci-hub.ru/10.1137/120880185
kaplan2013.pdf

-}

pattern Some :: a -> Maybe a
pattern Some x = Just x

pattern None :: Maybe a
pattern None = Nothing

data Pool k v = Pool { key :: k, items :: Concat [v] }
  deriving (Eq, Ord, Show, Functor, Foldable)
data Heap k v = Heap
  { left, right :: Maybe (Heap k v)
  , pool :: Pool k v
  } deriving (Eq, Ord, Show, Functor, Foldable)

-- For when None means positive infinity
_key :: Maybe (Heap k v) -> Inf Pos k
_key = maybe Infinity (Finite . key . pool)

-- assume: fmap (key . pool) . left < fmap (key . pool) . right
normaliseOrder :: Ord k => Heap k v -> Heap k v
normaliseOrder = \case
  x@Heap { left, right } | _key left > _key right
    -> x { left = right, right = left }
  x -> x



-- fill :: Ord k => Heap k v -> (Pool k v, Maybe (Heap k v))
-- fill = normaliseOrder >>> \case
--   Heap {pool, left=None} ->
--     (pool, None)
--   Heap {pool, left=Some left, right} ->
--     let (pool1, left') = fill left
--     in (pool, Just $ normaliseOrder $ Heap {left = left', right, pool = pool1})

-- Semigroup instance for Maybe a
-- combine :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a

-- Unfortunately, this messes with even and odd ranks, because it moves a whole 
-- tree closer to the root
-- So double even is harder?
-- Well, the paper says we don't need to worry about this too much, as we can 
-- still prove stuff, it's just a bit harder.
instance Ord k => Semigroup (Heap k v) where
  left <> right | (key . pool) left > (key . pool) right = right <> left
  left0 <> right =
    let (pool, left) = fill_ left0
    in normaliseOrder $ Heap {left, right = Some right, pool}

fill_ :: Ord k => Heap k v -> (Pool k v, Maybe (Heap k v))
fill_ Heap {pool, left, right} = (pool, left <> right)

{-

double even fill:

fill that pulls up
and sometimes we do it twice (recursively)

-- | double even fill
defill :: Ord k => Heap k v -> Heap k v
defill = fill >>> \case
  x@Heap { rank, left = Some _} | rank > t && even rank
    -> fill x
  x -> x

We can ignore the non-empty constraint in the paper.

de-fill only above threshold means: we use exact heaps below the threshold.
(We could even keep things sorted perfectly?)

So we can use a different data structure perhaps?  Maybe.

Above, we double fill every other.  Perhaps use something like red-black?

-}

-- fill double even?

{-
Min heap

Each node also has
a nonnegative integer rank, which is one less than the rank of its parent if it has a
parent.

Ie rank counts levels.

-}

-- data Heap k v  = Heap { rank :: Int, left :: Maybe (Heap k v), right :: Maybe (Heap k v), pool :: Pool k v }

-- data Pool k v = Pool { key :: k, items :: Concat [(k, v)]}

-- -- We have a conflict between empty pools having an infinite key
-- -- for one purpose, but having -infinity key when we want to combine pools with <>.

-- instance Ord k => Semigroup (Pool k v) where
--   Pool k xs <> Pool k' xs' = Pool (max k k') (xs <> xs')

-- toInf :: Maybe k -> Inf p k
-- toInf Nothing = Infinity
-- toInf (Just x) = Finite x

-- {- To simplify various tests, we define
-- null.key = ∞,
-- null.rank = ∞,
-- null.next = null, -- just (=<<)?
-- and null.items = null
-- -}

-- _key :: Maybe (Heap k v) -> Inf Pos k
-- _key = maybe Infinity (key . pool)

-- -- _rank :: Maybe (Heap k v) -> Inf Pos Int
-- -- _rank = toInf . fmap rank

-- -- _pool :: Maybe (Heap k v) -> Pool k v
-- -- _pool = maybe (Pool Infinity mempty) pool

-- -- insert :: Ord a => a -> Heap a -> Heap a
-- -- insert x (Heap r l r' xs) = merge (Heap 0 Nothing Nothing (x :| [])) (Heap r l r' xs)

-- -- merge :: Ord a => Heap a -> Heap a -> Heap a
-- -- merge h@(Heap r l r' xs) h'@(Heap r' l' r'' xs') =
-- --   if r < r' then Heap (r + 1) (Just h) (Just h') xs
-- --   else Heap (r' + 1) (Just h') (Just h) xs'

-- newtype SoftHeap k v = SoftHeap [Heap k v]

-- {-

-- We represent a soft heap by a singly linked list of roots of node-disjoint binary trees of
-- distinct ranks in findable order.

-- -}

-- -- TODO: This should probably become a parameter.
-- t :: Int
-- t = ceil (log (3 / epsilon))

-- epsilon = 1/3

-- -- t = ceil (log (3 / epsilon))
-- -- for epsilon = 1/3, we get t = 3

-- -- bubbleUp :: Ord k => Heap k v -> (Pool k v, Maybe (Heap k v))

-- {-

-- We maintain the roots in findable order : a root x
-- of minimum key is first, followed by the roots of rank less than x.rank in increasing
-- order by rank, followed by the roots of rank greater than x.rank in findable order.

-- ^ findable order is defined recursively, it seems.

-- meld: combine two soft heaps of lower rank, and combine them by filling.

-- -}

-- meld :: Ord k => Maybe (Heap k v) -> Maybe (Heap k v) -> Heap k v
-- meld left right | key right < key left = meld right left
-- meld _ _ = undefined

-- {-
-- extract: take pool out of a heap;
-- Then build a new heap by melding left and right.
-- -}

-- -- Unary nodes have a left child, and no right child.

-- -- moveUpLeft :: Ord k => Heap k v -> Heap k v
-- -- moveUpLeft x@Heap { left = Some left@{pool = p1}}, pool = p0 } =
-- --   x { left = Some left { pool = empty p1} --- ...

-- {-
-- Function fill(x)
-- if x.left.key > x.right.key then
--   x.left ↔ x.right
-- --> rerun with left and right flipped
-- --> this also ensures that left child is only empty, when
-- --> right child is also empty. 'left is empty' <= 'right is empty'.

-- x.key ← x.left.key
-- if x.set = null then
--   x.set ← x.left.set
-- else
--   x.set.next ↔ x.left.set.next
-- x.left.set ← null

-- ^ The above merges the two sets (parent and left child),
--   and then empties the left set, and merges their keys, too.

-- -- Ok, here we decide whether we need to recurse.
-- if x.left.left = null then
--   destroy x.left
--   x.left ← x.right
--   x.right ← null
-- else
--   defill(x.left)
-- -}

-- extractLeft :: Ord k => Heap k v -> (Pool k v, Maybe (Heap k v))
-- extractLeft h@Heap { left = Nothing, pool } = (pool, Nothing)
-- extractLeft h@Heap { left = Some left, pool = p0 } = (p0, undefined)-- h { left = left { pool = empty (pool left) }})

-- fill :: Ord k => Heap k v -> Heap k v
-- fill = normaliseOrder >>> \case
--   x@Heap { left = Some left, right, pool = p0 } ->
--     let (p1, left') = extractLeft left
--     in normaliseOrder $ x { left = left', pool = p0 <> p1 }

--   -- x@Heap {left, right} ->
--   --   let x' = x { key = _key left, items = items x <> _items left, left =  }

-- fib n =
