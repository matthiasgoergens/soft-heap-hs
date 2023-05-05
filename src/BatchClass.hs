module BatchClass where
import GHC.Exts qualified as G

-- We only really need pure, not sure whether <*> makes sense?
-- Look at laws perhaps.
class (forall a . Semigroup (b a), Applicative b, forall a . G.IsList (b a))
        => Batch b where
    -- This is like FactorialMonoid, sort of.
    pop :: b a -> Maybe (b a)
    size :: b a -> Int
    -- This is like pure?  Does <*> make any sense?
    -- <*> would have the sizes either add or multiply, I guess.
    key :: b a -> a

data B a = B { _size :: Int, _key :: a }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance G.IsList (B a) where
    type Item (B a) = a
    -- This one fails for empty lists.
    fromList = foldr1 (<>) . fmap pure
    -- This one is not very useful.
    toList = pure . key

instance Applicative B where
    pure = B 1
    f <*> a = B (_size f * _size a) (_key f $ _key a)

instance Semigroup (B a) where
    a <> b = B (_size a + _size b) (_key b)

instance Batch B where
    size = _size
    key = _key
    pop B {_key, _size} | _size > 1 = Just B {_key, _size = _size - 1 }
    pop _ = Nothing

{-
pure id <*> x == x

size (pure x) == 1
Means: size needs to multiply for (<*>)
-}
