# A python implementation of the soft heaps described in the paper
# "Soft Heaps Simplified", by Haim Kaplan, Robert E. Tarjan and Uri Zwick.
# (c) Haim Kaplan, Robert E. Tarjan and Uri Zwick.

import random
from dataclassy import dataclass
import dataclassy as d
import math
from typing import Union
INF = math.inf
T = INF
# T = 2

########
# Item #
########


@d.dataclass()
class Item:
    key: float
    next: 'Item' = None

    def __post_init__(self):
        if self.next is None:
            self.next = self

nullItem = Item(INF)

########
# Node #
########


@d.dataclass
class Node:
    set: Item = nullItem
    key: float = INF
    rank: int
    left: Union[None, 'Node'] = None
    right: Union[None, 'Node'] = None

    next: 'Node' = None

    def __post_init__(self):
        if self.next is None:
            self.next = self

null = Node(rank=INF)
null.left = null
null.right = null
# null.set = nullItem
# null.key = INF
# null.rank = INF
# null.left = null
# null.right = null
# null.next = null

##########
# defill #
##########


def defill(x: Node):
    fill(x)
    if x.rank > T and x.rank % 2 == 0 and x.left != null:
        fill(x)

########
# fill #
########


def fill(x):
    if x.left.key > x.right.key:
        x.left, x.right = x.right, x.left
    x.key = x.left.key
    if x.set == nullItem:
        x.set = x.left.set
    else:
        x.set.next, x.left.set.next = x.left.set.next, x.set.next
    x.left.set = nullItem
    if x.left.left == null:
        # destroy x.left
        x.left = x.right
        x.right = null
    else:
        defill(x.left)

#############
# make_heap #
#############


def make_heap():
    return null

############
# find_min #
############


def find_min(H):
    return (H.set.next, H.key)

#############
# rank_swap #
#############


def rank_swap(H):
    x = H.next
    if H.rank <= x.rank:
        return H
    else:
        H.next = x.next
        x.next = H
        return x

############
# key_swap #
############


def key_swap(H):
    x = H.next
    if H.key <= x.key:
        return H
    else:
        H.next = x.next
        x.next = H
        return x

##############
# delete_min #
##############


def delete_min(H):
    e = H.set.next
    if e.next != e:
        H.set.next = e.next
        return H
    else:
        H.set = nullItem
        k = H.rank
        if H.left == null:
            L = H.next
            # destroy H
            H = L
        else:
            defill(H)
        return reorder(H, k)

###########
# reorder #
###########


def reorder(H, k):
    if H.next.rank < k:
        H = rank_swap(H)
        H.next = reorder(H.next, k)
    return key_swap(H)

##########
# insert #
##########


def insert(e, H):
    return key_swap(meldable_insert(make_root(e), rank_swap(H)))

#############
# make_root #
#############


def make_root(e):
    return Node(set=e, key=e.key, rank=0, left=null, right=null, next=null)

###################
# meldable_insert #
###################


def meldable_insert(x, H):
    if x.rank < H.rank:
        x.next = key_swap(H)
        return x
    else:
        return meldable_insert(link(x, H), rank_swap(H.next))

########
# link #
########


def link(x, y):
    z = Node(set=nullItem, key=x.key, rank=x.rank + 1, left=x, right=y, next=null)
    z.set = nullItem
    defill(z)
    return z

########
# meld #
########


def meld(H1, H2):
    return key_swap(meldable_meld(rank_swap(H1), rank_swap(H2)))

#################
# meldable_meld #
#################


def meldable_meld(H1, H2):
    if H1.rank > H2.rank:
        H1, H2 = H2, H1
    if H2 == null:
        return H1
    else:
        return meldable_insert(H1, meldable_meld(rank_swap(H1.next), H2))

#################################################################


def randlist(n):
    return [random.random() for i in range(n)]


def randperm(n):
    return random.sample(list(range(n)), n)


def build(lst):
    P = make_heap()
    for it in lst:
        P = insert(Item(it), P)
    return P


def extract(P):
    lst = []
    while P != null: # TODO(matthias): or nullItem?
        lst.append(find_min(P)[0].key)
        P = delete_min(P)
    return lst


def sort(lst):
    print(lst)
    P = build(lst)
    lst1 = extract(P)
    if T == INF:
        for i in range(1, len(lst)):
            if lst1[i] < lst1[i-1]:
                print("BUG!!!")
                raise BUG()
    print(lst1)
    print(" ")
    return lst1


sort(randperm(100))

T = 3
sort(randperm(100))

T = INF
P = build(randperm(100))
Q = build(randperm(200))
print(extract(meld(P, Q)))
