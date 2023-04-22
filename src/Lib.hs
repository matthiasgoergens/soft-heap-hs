module Lib where
import Control.Arrow ((>>>))

import Data.List.NonEmpty qualified as N
-- import Data.List.NonEmpty (NonEmpty)

import Data.Monoid.Instances.Concat
import Data.Monoid.Inf (Inf(..), Pos)
import Data.Monoid.Null (MonoidNull(null))
import Data.Function (on)

{-
SOFT HEAPS SIMPLIFIED
https://sci-hub.ru/10.1137/120880185
kaplan2013.pdf

-}

pattern Some :: a -> Maybe a
pattern Some x = Just x
pattern None :: Maybe a
pattern None = Nothing

{-
Min heap

Each node also has
a nonnegative integer rank, which is one less than the rank of its parent if it has a
parent.

Ie rank counts levels.

-}

data Heap k v  = Heap { rank :: Int, left :: Maybe (Heap k v), right :: Maybe (Heap k v), pool :: Pool k v }

data Pool k v = Pool { key :: k, items :: Concat [(k, v)]}

-- We have a conflict between empty pools having an infinite key
-- for one purpose, but having -infinity key when we want to combine pools with <>.

instance Ord k => Semigroup (Pool k v) where
  Pool k xs <> Pool k' xs' = Pool (max k k') (xs <> xs')

toInf :: Maybe k -> Inf p k
toInf Nothing = Infinity
toInf (Just x) = Finite x

{- To simplify various tests, we define
null.key = ∞,
null.rank = ∞,
null.next = null, -- just (=<<)?
and null.items = null
-}

_key :: Maybe (Heap k v) -> Inf Pos k
_key = maybe Infinity (key . pool)

-- _rank :: Maybe (Heap k v) -> Inf Pos Int
-- _rank = toInf . fmap rank

-- _pool :: Maybe (Heap k v) -> Pool k v
-- _pool = maybe (Pool Infinity mempty) pool

-- insert :: Ord a => a -> Heap a -> Heap a
-- insert x (Heap r l r' xs) = merge (Heap 0 Nothing Nothing (x :| [])) (Heap r l r' xs)

-- merge :: Ord a => Heap a -> Heap a -> Heap a
-- merge h@(Heap r l r' xs) h'@(Heap r' l' r'' xs') =
--   if r < r' then Heap (r + 1) (Just h) (Just h') xs
--   else Heap (r' + 1) (Just h') (Just h) xs'

newtype SoftHeap k v = SoftHeap [Heap k v]

{-

We represent a soft heap by a singly linked list of roots of node-disjoint binary trees of
distinct ranks in findable order.

-}

-- TODO: This should probably become a parameter.
t :: Int
t = ceil (log (3 / epsilon))

epsilon = 1/3

-- t = ceil (log (3 / epsilon))
-- for epsilon = 1/3, we get t = 3

-- bubbleUp :: Ord k => Heap k v -> (Pool k v, Maybe (Heap k v))

{-

We maintain the roots in findable order : a root x
of minimum key is first, followed by the roots of rank less than x.rank in increasing
order by rank, followed by the roots of rank greater than x.rank in findable order.

^ findable order is defined recursively, it seems.

meld: combine two soft heaps of lower rank, and combine them by filling.

-}

-- | double even fill
defill :: Ord k => Heap k v -> Heap k v
defill = fill >>> \case
  x@Heap { rank, left = Some _} | rank > t && even rank
    -> fill x
  x -> x

normaliseOrder :: Ord k => Heap k v -> Heap k v
normaliseOrder = \case
  x@Heap { left, right }
    | _key left > _key right -> x { left = right, right = left }
  x -> x

meld :: Ord k => Maybe (Heap k v) -> Maybe (Heap k v) -> Heap k v
meld left right | key right < key left = meld right left
meld 

{-
extract: take pool out of a heap;
Then build a new heap by melding left and right.
-}

-- Unary nodes have a left child, and no right child.

-- moveUpLeft :: Ord k => Heap k v -> Heap k v
-- moveUpLeft x@Heap { left = Some left@{pool = p1}}, pool = p0 } =
--   x { left = Some left { pool = empty p1} --- ...

{-
Function fill(x)
if x.left.key > x.right.key then
  x.left ↔ x.right
--> rerun with left and right flipped
--> this also ensures that left child is only empty, when
--> right child is also empty. 'left is empty' <= 'right is empty'.

x.key ← x.left.key
if x.set = null then
  x.set ← x.left.set
else
  x.set.next ↔ x.left.set.next
x.left.set ← null
^ The above merges the two sets (parent and left child),
  and then empties the left set, and merges their keys, too.

-- Ok, here we decide whether we need to recurse.
if x.left.left = null then
  destroy x.left
  x.left ← x.right
  x.right ← null
else
  defill(x.left)
-}

extractLeft :: Ord k => Heap k v -> (Pool k v, Maybe (Heap k v))
extractLeft h@Heap { left = Nothing, pool } = (pool, Nothing)
extractLeft h@Heap { left = Some left, pool = p0 } = (p0, undefined)-- h { left = left { pool = empty (pool left) }})

fill :: Ord k => Heap k v -> Heap k v
fill = normaliseOrder >>> \case
  x@Heap { left = Some left, right, pool = p0 } ->
    let (p1, left') = extractLeft left
    in normalizeOrder $ x { left = left', pool = p0 <> p1 }

  
  -- x@Heap {left, right} ->
  --   let x' = x { key = _key left, items = items x <> _items left, left =  }
