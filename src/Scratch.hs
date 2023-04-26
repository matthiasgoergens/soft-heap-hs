module Scratch where

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

-- We don't necessarily need to store the capacity with each node, we could store it globally.
-- In Rust we could use a presized vector?
data Capped a = Capped { capacity :: !Int, getCapped :: [a]}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

newtype Exact a = Exact [a]
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Semigroup, Monoid)

insertE :: Ord a => a -> Capped a -> Either (Exact a) (Capped a)
insertE x (Capped cap xs) =
  let l = DL.insert x xs
  in if cap <= length l
    then Left (Exact l)
    else Right (Capped cap l)

-- pop :: Exact a -> (Maybe a, Exact a)
-- pop Exact { getExact = [] } = (Nothing, Exact 0 [])
-- pop Exact { getExact = x:xs } = (Just x, Exact (length xs) xs)

-- data Colour = R | B
--   deriving (Eq, Ord, Show)

newtype Pool a = Pool { items :: Concat [a] }
  deriving (Eq, Ord, Show, Functor, Foldable)

deriving instance Semigroup (Pool a)
deriving instance Monoid (Pool a)
deriving instance MonoidNull (Pool a)

instance Applicative Pool where
  pure = Pool . pure . pure
  Pool fs <*> Pool xs = Pool $ (<*>) <$> fs <*> xs

class Pull1 h where
  pull1 :: Ord a => h a -> Maybe (a, h a)

-- We can probably later turn everything into monoids and their subclasses.
-- Especially pull1 looks like splitPrimePrefix
-- What about pull?

-- Perhaps this should be something like factorial monoid?
instance Pull1 Exact where
  pull1 (Exact []) = Nothing
  pull1 (Exact (x:xs)) = Just (x, Exact xs)

-- OK, we decide that we have a copy of k?  OK!
instance Pull1 Pool where
  pull1 p@Pool {items} = splitPrimePrefix items <&>
    \(x, xs) -> (head $ force x, p {items = xs})

-- This one only happens at the top.
instance Pull1 Heap where
  -- pull1 does not recurse into Nodes.
  pull1 (Leaf e) = (fmap . fmap) Leaf (pull1 e)
  pull1 Node {left, right, key, pool} = case pull1 pool of
    Just (x, pool') -> Just (x, Node {left, right, key, pool = pool'})
    -- pool was empty:
    Nothing -> Just . (key,) $ normaliseOrder $
      case pull True left of
        Nothing -> Leaf (Exact [])
        Just ((key', pool'), left') ->
          Node {left = left', right = right, key = key', pool = pool'}

-- fill vs pull.
-- fill = undefined

-- fill almost like in paper.
-- fill :: Ord a => Heap a -> Heap a
-- fill = undefined

class Pull h where
  pull :: Ord a => Bool -> h a -> Maybe ((a, Pool a), h a)

instance Pull Exact where
  pull _ (Exact []) = Nothing
  pull _ (Exact (x:xs)) = Just ((x, mempty), Exact xs)

injectSmaller :: (a, Pool a) -> Heap a -> Heap a
injectSmaller (key, pool) = \case
  left@Leaf {} -> Node {left, right=left, key, pool}
  Node {left, right, key = key', pool = pool'} ->
    Node { left, right, key = key', pool = pool' <> pool <> pure key }

fill :: Ord a => Heap a -> Heap a
fill = pull False >>> \case
  Nothing -> Leaf (Exact [])
  Just (v, node) -> injectSmaller v node

instance Pull Heap where
  pull even = (fmap . fmap) normaliseOrder <<< normaliseOrder >>> \case
    Leaf exact -> fmap Leaf <$> pull even exact
    Node {left, right, key, pool} ->
      case pull (not even) left of
        -- right should be empty, too.  Because of normalisation.
        Nothing -> Just ((key, pool), right)
        Just ((key', pool'), left') ->
          Just ((key, pool), doubleFill $ Node {left = left', right, key=key', pool = pool'})
    where doubleFill = bool id fill even

newtype BHeap a = BHeap (Bool -> Heap a)

-- instance Ord a => Semigroup (BHeap a) where
--   (<>) (BHeap left0) (BHeap right) = BHeap $ \case
--     even
--       | _key (left0 even) > _key (right even) -> right <> left0
--       | otherwise ->
--         let (pool, left) = fill (left0 even)
--         in normaliseOrder $ Node {left, right = right even, pool, key=undefined}

meld :: Ord a => Bool -> Heap a -> Heap a -> Heap a
meld even left right | _key left > _key right = meld even right left
meld even left right = normaliseOrder $
  case pull even left of
    -- right should also be empty in this case, because we normalised our order.
    Nothing -> Leaf (Exact [])
    Just ((key, pool), left') ->
      Node {left = left', right, key, pool}
  -- normaliseOrder $ Node {left, right, key=undefined, pool=undefined}

-- doubledPull :: Ord a => Heap a -> Heap a
-- doubledPull = normaliseOrder >>> \case
--   l@Leaf {} -> l
--   n@Node {left, right, key, pool} ->
--     case pull True left of
--       Nothing -> n
--       Just ((key', pool'), left') ->
--         Node {left = left', right, key = key', pool = pool' <> pure key}

-- pull' :: Ord a => [Heap a] -> (Maybe (a, Pool a), [Heap a])
-- pull' = sortBy (compare `on` _key) >>> \case
--   x:xs -> case pull x
--   [] -> (None, [])

normaliseOrder :: Ord a => Heap a -> Heap a
normaliseOrder = \case
  x@Node { left, right } | _key left > _key right
    -> x { left = right, right = left }
  x -> x

-- Leaves are black.
data Heap a =
  Leaf (Exact a)
  | Node
  { left :: Heap a
  , right :: Heap a
  , key :: a
  , pool :: Pool a
  } deriving (Eq, Ord, Show, Functor, Foldable)

-- findable-order
-- meldable-order0

data SH a = SH { nursery :: Capped a, heaps :: [Heap a] }

emptyH :: Heap a
emptyH = Leaf (Exact [])

empty :: Int -> SH a
empty size = SH { nursery = Capped size [], heaps = [] }

insert :: Ord a => a -> SH a -> SH a
insert a SH { nursery, heaps } = case insertE a nursery of
  Right capped -> SH { nursery = capped, heaps }
  -- Left (x, exact) ->
  --   let n = Node { left = left

-- single fill.
-- fill :: Ord a => Heap a -> Heap a
-- fill (Heap a) = case a of
  -- Left (Exact _ []) -> error "empty exact"
  -- Left (Exact _ [x]) -> Heap (Left (Exact 1 [])) (Left (Exact 1 [])) (Pool x mempty)
  -- Left (Exact _ (x:y:xs)) -> Heap (Left (Exact 1 [])) (Left (Exact 1 [])) (Pool x (y :| xs))
  -- Right (Heap l r p) -> Heap (Right (fill l)) (Right (fill r)) p

-- For when None means positive infinity
_key :: Heap a -> Inf Pos a
_key (Leaf (Exact [])) = Infinity
_key (Leaf (Exact (x:_))) = Finite x
_key Node {key} = Finite key
