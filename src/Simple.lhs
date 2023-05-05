> module Simple where
> import BatchClass qualified as BC
> import Data.Ord (comparing)
> import Control.Arrow ((>>>), (<<<))

unc is a container to wrap uncorrupted in.  Needs (<>) and pure or so.

We can use a difference list, or similar.

> data Cons unc b a = Cons { batch :: b a, uncorrupted :: unc a, rest :: MCons unc b a }
> type MCons unc b a = Maybe (Cons unc b a)

Now we need to do something like merge from merge-sort to produce a new stream of Cons.

And we also need to fuse batches.

Keep in mind that `uncorrupted` and `batch` are basically 'exclusive' choices.  Sort of.

> merge :: (BC.Batch b, Ord a, Semigroup (unc a), Applicative unc) => MCons unc b a -> MCons unc b a -> MCons unc b a
> merge Nothing Nothing = Nothing
> merge (Just x) Nothing = Just x
> merge Nothing (Just y) = Just y
> merge (Just x@(Cons bx ux rx)) (Just y@(Cons by uy ry)) = Just $ case comparing BC.key bx by of
>   GT -> Cons by (pure (BC.key bx) <> ux <> uy) (merge (Just x) ry)
>   _  -> Cons bx (ux <> pure (BC.key by) <> uy) (merge rx (Just y))

For this to work, uncorrupted must be only the 'hidden' uncorrupted,
ie not including the current batch key.

But the also means we need to fix merge above.

> fuse :: (BC.Batch b, Semigroup (unc a)) => MCons unc b a -> MCons unc b a
> fuse = fmap \case
>   Cons bx ux (Just (Cons by uy ry)) -> Cons (bx <> by) (ux <> uy) (fuse ry)
>   x -> x
