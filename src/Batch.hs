module Batch where
import Data.Bool (bool)
import Control.Arrow ((>>>), (<<<))
import Data.List qualified as DL
import Data.List.NonEmpty qualified as N
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid.Instances.Concat
import Data.Monoid.Instances.Concat ( force )
import Data.Monoid.Factorial ( splitPrimePrefix, splitPrimeSuffix )
import Data.Monoid.Inf (Inf (..), Pos)
import Data.Maybe (listToMaybe)
import Data.Functor
import Data.Monoid.Null
import Data.Semigroup.Factorial
import Prelude hiding (null, even)
import Data.Coerce
import Data.Function (on)
import Data.Monoid
import Data.Semigroup hiding (First)
import Data.Monoid.Null
import Data.Monoid.Factorial


-- Any factorial monoid will do?
-- Empty Batch?
newtype Batch a = Batch (Concat [a])
    deriving (Eq, Ord, Show, Functor, Foldable, Semigroup, Monoid, MonoidNull, FactorialMonoid, Factorial)
    -- Note: Ord instances only really works when we don't have any ties, as 
    -- then we would descend arbitraliy deep into the batch, which costs too 
    -- much.

{-
When combining batches with <>, the smaller one needs to go to the back,
so that comparing by front works.
-}

-- Ord compares from front, so we need to pop1 at back.
pop1 :: Batch a -> Maybe (Batch a)
pop1 = fmap snd . splitPrimeSuffix

-- normalise :: Batch a -> Batch a
-- normalise (Batch () | n < 0 = mempty
-- normalise b = b

-- instance Applicative Batch where
--     pure = Batch . (mempty,) . pure
--     -- Not sure if this one makes sense?  But we need the pure instance.
--     Batch (x, f) <*> Batch (y, a) = normalise $ Batch (x <> y, f <*> a)

-- pop1 :: Batch a -> Batch a
-- pop1 = normalise >>> \(Batch (Sum n, a)) -> Batch (Sum (n-1), a)
