# soft-heap-hs

monoid-subclasses to get `Data.Monoid.Instances.Concat`

> Concat is a transparent monoid transformer. The behaviour of the Concat a instances of monoid subclasses is identical to the behaviour of their a instances, up to the pure isomorphism.

> The only purpose of Concat then is to change the performance characteristics of various operations. Most importantly, injecting a monoid into Concat has the effect of making mappend a constant-time operation. The splitPrimePrefix and splitPrimeSuffix operations are amortized to constant time, provided that only one or the other is used. Using both operations alternately will trigger the worst-case behaviour of O(n).

That's what we need for https://sci-hub.ru/10.1137/120880185 "Soft Heaps simplified." (2013)
