> module Simple where
> import Data.Ord (comparing)
> import Control.Arrow ((>>>))
> import Data.List qualified as DL
> import Data.Maybe (catMaybes)
> import Data.Monoid qualified as DM
> import Data.Functor ((<&>))
> import BatchHybrid qualified as B

unc is a container to wrap uncorrupted in.  Needs Monoid and pure or so.

We can use a difference list, or similar.

> data Cons unc pool a = Cons { batch :: B.Batch pool a, uncorrupted :: unc a, rest :: MCons unc pool a }
>  deriving (Eq, Ord, Show, Functor)
> type MCons unc b a = Maybe (Cons unc b a)

> singleton :: (Applicative (B.Batch pool), Monoid (unc a)) => a -> Cons unc pool a
> singleton a = Cons { batch = pure a, uncorrupted = mempty, rest = Nothing }

Now we need to do something like merge from merge-sort to produce a new stream of Cons.

And we also need to fuse batches.

Keep in mind that `uncorrupted` and `batch` are basically 'exclusive' choices.  Sort of.

> merge2 :: (Ord a, Semigroup (unc a), Applicative unc) => MCons unc b a -> MCons unc b a -> MCons unc b a
> merge2 Nothing Nothing = Nothing
> merge2 (Just x) Nothing = Just x
> merge2 Nothing (Just y) = Just y
> merge2 (Just x@(Cons bx ux rx)) (Just y@(Cons by uy ry)) = Just $ case comparing B.key bx by of

Hmm, do we just need to fix the latter 'uncorrupted',
or does our fix need to move recursively down the stream?

>   GT -> Cons by (pure (B.key bx) <> ux <> uy) (merge2 (Just x) ry)
>   _  -> Cons bx (ux <> pure (B.key by) <> uy) (merge2 rx (Just y))

For this to work, uncorrupted must be only the 'hidden' uncorrupted,
ie not including the current batch key.

But the also means we need to fix merge2 above to augment the uncorrupted when we zip things together.

> fuse :: (Monoid (B.Batch pool a), Semigroup (unc a)) => Cons unc pool a -> Cons unc pool a
> fuse (Cons bx _ (Just (Cons by uy ry))) = Cons (bx <> by) uy (fmap fuse ry)
> fuse x = x

Semigroup (unc a) would be enough.  But Monoid makes it more convenient.

> mergeN :: (Monoid (unc a), Ord a, Applicative unc) => [MCons unc b a] -> MCons unc b a
> mergeN = catMaybes >>> helper where
>   helper = DL.uncons >>> fmap \case
>     (Cons bx ux rx, xs) -> Cons bx (ux <> foldMap uncorrupted' xs) (helper $ insert' rx xs)
>   uncorrupted' (Cons b u _) = pure (B.key b) <> u
>   insert' = foldMap_ . DL.insertBy . comparing $ B.key . batch

> foldMap_ :: Foldable m => (a -> b -> b) -> m a -> (b -> b)
> foldMap_ f = DM.appEndo . foldMap (DM.Endo . f)

> pop :: (B.Pop (B.Batch pool)) => Cons unc pool a -> Maybe (Cons unc pool a)
> pop cons@(Cons b _ rest) = case B.pop b of
>   Nothing -> rest
>   Just b' -> Just $ cons {batch = b'}

> uncons :: (B.Uncons (B.Batch pool)) => Cons unc pool a -> (a, Maybe (Cons unc pool a))
> uncons cons@(Cons b _ rest) = B.uncons b <&> \case
>  Nothing -> rest
>  Just b' -> Just $ cons {batch = b'}
