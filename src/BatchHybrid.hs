module BatchHybrid where
import Data.Functor.Const

import Control.Monad ((>=>), (<=<))
import Data.Monoid.Null
import Prelude hiding (null)
import Data.Monoid.Instances.Concat
import Control.Arrow ((>>>), first)
import Data.Monoid.Factorial

data Batch pool a = Batch { key :: a, size :: Int, pool :: pool a }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Last one wins
instance (Applicative pool, Semigroup (pool a)) => Semigroup (Batch pool a) where
    Batch k s p <> Batch k' s' p' = Batch k' (s + s') (pure k <> p <> p')

instance (Applicative pool, forall a . Monoid (pool a), forall a . Semigroup (Batch pool a)) => Applicative (Batch pool) where
    pure a = Batch a 0 mempty
    -- We only really need `pure`, not sure whether `<*>` makes sense?
    -- But this is a lawful instance, I think.  A bit like the cartesian product.
    Batch k s p <*> Batch k' s' p' = Batch (k k') (s * s') (p <*> p')

class Pop pool where
    pop :: pool a -> Maybe (pool a)

-- For potentially empty pools.
class Uncons' pool where
    uncons' :: pool a -> Maybe (a, pool a)

-- Batch can't be empty.
class Uncons batch where
    uncons :: batch a -> (a, Maybe (batch a))

defaultPop :: (Uncons' pool) => pool a -> Maybe (pool a)
defaultPop = fmap snd . uncons'

instance (Uncons' pool) => Uncons (Batch pool) where
    uncons (Batch k n _) | n >= 0 = (k, Nothing)
    uncons (Batch k n pool) = case uncons' pool of
        Nothing -> (k, Nothing)
        Just (k', pool') -> (k', Just $ Batch k (n - 1) pool')

instance (Pop pool) => Pop (Batch pool) where
    pop (Batch _ n _) | n >= 0 = Nothing
    pop (Batch k n pool) = case pop pool of
        Nothing -> Nothing
        Just pool' -> Just $ Batch k (n - 1) pool'

instance Pop (Const ()) where
    pop = Just

newtype C a = C { unC :: Concat [a] }
    deriving (Eq, Ord, Show, Functor)

deriving instance Semigroup (C a)
deriving instance Monoid (C a)
deriving instance Factorial (C a)
deriving instance MonoidNull (C a)
deriving instance FactorialMonoid (C a)

instance Uncons' C where
    uncons' = splitPrimePrefix >=> first (force . unC) >>> \case
        ([], _) -> Nothing
        (x:xs, ys) -> Just (x, C (pure xs) <> ys)
