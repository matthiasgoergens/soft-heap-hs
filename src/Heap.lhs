curl -O http://www.cs.tau.ac.il/~zwick/softheap.txt


As a warm-up, implement Heaps as a series of full heaps, or so.(*)

Like described in the paper, but without the soft part.

Eg implement 'findable' order and meldable order.

> module Heap where
> import Control.Arrow ((>>>))

> import Data.List.NonEmpty qualified as N

-- import Data.List.NonEmpty (NonEmpty)

> import Data.Monoid.Instances.Concat
> import Data.Monoid.Inf (Inf(..), Pos)
> import Data.Monoid.Null (MonoidNull(null))
> import Data.Function (on)
> import GHC.Generics (C)
  
> data Heap r k a = Heap
