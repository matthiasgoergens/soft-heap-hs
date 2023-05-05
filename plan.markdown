https://sci-hub.ru/10.1137/120880185

"Soft Heaps simplified." (2013)

https://drops.dagstuhl.de/opus/volltexte/2019/10671/pdf/LIPIcs-ICALP-2019-95.pdf

"Dynamic Ordered Sets with Approximate Queries,
Approximate Heaps and Soft Heaps" (2012)

---

Shy heaps:

https://arxiv.org/pdf/math/0409028.pdf has a nice construction of the dual of the nested matroids (or freedom matroids.)  Starting on page 10, '6. Freedom matroids on ordered sets'

> Proposition 6.3. For any linearly ordered S, and T ⊆ S, the dual MT (S)∗ of the matroid MT (S) is equal to MT ′ (Sφ), where T ′ is the complement of T in S and Sφ is the reversal of S. In particular, the class of freedom matroids is closed under duality.

Set T are the thresholds.  We basically insert a 'keep smallest' before every element in T.  This works as a bit-map.  Proposition 6.3 tells you to negate the bitmap, and reverse the order to get the dual.

That's important for my shy-heap, where this is an important step.

Example 6.5 even brings up a bitmap table.

They use that table to count the number of freedom matroids (as 2^n).

---

https://hackage.haskell.org/package/monoid-subclasses-1.2.3/docs/Data-Monoid-Instances-Concat.html seems to work?

> Concat is a transparent monoid transformer. The behaviour of the Concat a instances of monoid subclasses is identical to the behaviour of their a instances, up to the pure isomorphism.

> The only purpose of Concat then is to change the performance characteristics of various operations. Most importantly, injecting a monoid into Concat has the effect of making mappend a constant-time operation. The splitPrimePrefix and splitPrimeSuffix operations are amortized to constant time, provided that only one or the other is used. Using both operations alternately will trigger the worst-case behaviour of O(n).
---
see https://raw.githubusercontent.com/millimat/Soft-Heap/master/soft-heaps-an-intuitive-overview.pdf
---

Also https://citeseerx.ist.psu.edu/viewdoc/download?rep=rep1&type=pdf&doi=10.1.1.46.1458 'Construction Red Black Trees' by Ralf Hinze

Two conditions:

1 A red node has a black parent
2 All paths from root to leave have the same number of black nodes.

We are interested in the first condition (and perhaps a weaker version of the second as well?) to do our double-even filling.  Red nodes don't get double-filled.

  In our case, we build our exact-trees once.  Once they enter under corrupt trees, we only pull single elements in sorted order from them.  (So our exact trees might as well be sorted lists.)

Perhaps we should pretend, for our balancing criterion, that our exact-leaves never go completely empty?  That they leave ghosts behind?

---

Problem is that promoting a red node to a black spot (via filling) messes up colouring below.  Or does it?  Hmm, no, I guess not?

-- Red black trees for heaps?

We never insert into our corrupt trees.  We either meld two of them, or we pop.

---

There's something in the findable/meldable order that smells a bit like cartesian trees.  But not sure.

---

Does our amortised cost suffer, if we use a dense representation of digits?  Or does that wash out?

---

Nursery:

insert, pop-min.

Insert can lead to being full, and thus sealing.

Heap-tree: no more insert.  Only got: pop-min-batch and melding of two trees.

Technically, we also have a pop-min; but we do that from a batch.

So perhaps we should talk about pennants (topped trees), so that only the top needs to support single pop-min.

What about double-fill?  Some nodes pull twice as much as others.  (That's a bit like the red-black tree, where some nodes are red, and others are black.  Can we use that construction?)
