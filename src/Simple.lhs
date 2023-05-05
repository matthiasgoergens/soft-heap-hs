> module Simple where
> import BatchClass qualified as BC
> import Data.Ord (comparing)
> import Control.Arrow ((>>>), (<<<), second)
> import Data.List qualified as DL
> import Data.Maybe (catMaybes)
> import Data.Foldable (foldMap)
> import Data.Monoid qualified as DM
> import Data.Function ((&))
> import Data.Functor ((<&>))

unc is a container to wrap uncorrupted in.  Needs Monoid and pure or so.

We can use a difference list, or similar.

> data Cons unc b a = Cons { batch :: b a, uncorrupted :: unc a, rest :: MCons unc b a }
> type MCons unc b a = Maybe (Cons unc b a)

> singleton :: (BC.Batch b, Monoid (unc a)) => a -> Cons unc b a
> singleton a = Cons { batch = pure a, uncorrupted = mempty, rest = Nothing }

Now we need to do something like merge from merge-sort to produce a new stream of Cons.

And we also need to fuse batches.

Keep in mind that `uncorrupted` and `batch` are basically 'exclusive' choices.  Sort of.

> merge2 :: (BC.Batch b, Ord a, Semigroup (unc a), Applicative unc) => MCons unc b a -> MCons unc b a -> MCons unc b a
> merge2 Nothing Nothing = Nothing
> merge2 (Just x) Nothing = Just x
> merge2 Nothing (Just y) = Just y
> merge2 (Just x@(Cons bx ux rx)) (Just y@(Cons by uy ry)) = Just $ case comparing BC.key bx by of

Hmm, do we just need to fix the latter 'uncorrupted',
or does our fix need to move recursively down the stream?

>   GT -> Cons by (pure (BC.key bx) <> ux <> uy) (merge2 (Just x) ry)
>   _  -> Cons bx (ux <> pure (BC.key by) <> uy) (merge2 rx (Just y))

For this to work, uncorrupted must be only the 'hidden' uncorrupted,
ie not including the current batch key.

But the also means we need to fix merge2 above to augment the uncorrupted when we zip things together.

> fuse :: (BC.Batch b, Semigroup (unc a)) => Cons unc b a -> Cons unc b a
> fuse (Cons bx _ (Just (Cons by uy ry))) = Cons (bx <> by) uy (fmap fuse ry)
> fuse x = x

Semigroup (unc a) would be enough.  But Monoid makes it more convenient.

> mergeN :: (Monoid (unc a), BC.Batch b, Ord a, Applicative unc) => [MCons unc b a] -> MCons unc b a
> mergeN = catMaybes >>> helper where
>   helper = DL.uncons >>> fmap \case
>     (Cons bx ux rx, xs) -> Cons bx (ux <> foldMap uncorrupted' xs) (helper $ insert' rx xs)
>   uncorrupted' (Cons b u _) = pure (BC.key b) <> u
>   insert' = foldMap_ . DL.insertBy . comparing $ BC.key . batch

> foldMap_ :: Foldable m => (a -> b -> b) -> m a -> (b -> b)
> foldMap_ f = DM.appEndo . foldMap (DM.Endo . f)

> pop :: BC.Batch b => Cons unc b a -> Maybe (Cons unc b a)
> pop cons@(Cons b _ rest) = case BC.pop b of
>   Nothing -> rest
>   Just b' -> Just $ cons {batch = b'}

> uncons :: BC.BatchGet b => Cons unc b a -> (a, Maybe (Cons unc b a))
> uncons cons@(Cons b _ rest) = BC.uncons b <&> \case
>  Nothing -> rest
>  Just b' -> Just $ cons {batch = b'}
