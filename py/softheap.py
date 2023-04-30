# A python implementation of the soft heaps described in the paper
# "Soft Heaps Simplified", by Haim Kaplan, Robert E. Tarjan and Uri Zwick.
# (c) Haim Kaplan, Robert E. Tarjan and Uri Zwick.

import random
from dataclasses import dataclass
import dataclasses as d
import math
from typing import Union, Tuple, List
from copy import copy
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

@dataclass()
class Node:
    set: Item = d.field(default_factory=lambda : nullItem)
    key: float = INF
    rank: Union[float,int] = INF
    left: 'Node' = None
    right: 'Node' = None

    next: 'Node' = None

    def __post_init__(self):
        if self.next is None:
            self.next = self
        # This is a hack to get around the fact that dataclasses don't
        # support default values for fields that are instances of the
        # class being defined.
        if self.left is None:
            try:
                self.left = null
            except NameError:
                self.left = self
        if self.right is None:
            try:
                self.right = null
            except NameError:
                self.left = self

null = Node(rank=INF)
# null.left = null
# null.right = null
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
    "double even fill"
    fill(x)
    if x.rank > T and x.rank % 2 == 0 and x.left != null:
        fill(x)

########
# fill #
########


def fill(x: Node):
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


def make_heap() -> Node:
    return null

############
# find_min #
############


def find_min(H) -> Tuple[Item, float]:
    return (H.set.next, H.key)

#############
# rank_swap #
#############


def rank_swap(H: Node) -> Node:
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


def key_swap(H: Node) -> Node:
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


def delete_min(H: Node) -> Node:
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


def reorder(H: Node, k: int) -> Node:
    if H.next.rank < k:
        H = rank_swap(H)
        H.next = reorder(H.next, k)
    return key_swap(H)

##########
# insert #
##########


def insert(e: Item, H: Node) -> Node:
    return key_swap(meldable_insert(make_root(e), rank_swap(H)))

#############
# make_root #
#############


def make_root(e: Item) -> Node:
    return Node(set=e, key=e.key, rank=0, left=null, right=null, next=null)

###################
# meldable_insert #
###################


def meldable_insert(x: Node, H: Node) -> Node:
    if x.rank < H.rank:
        x.next = key_swap(H)
        return x
    else:
        return meldable_insert(link(x, H), rank_swap(H.next))

########
# link #
########


def link(x: Node, y: Node) -> Node:
    z = Node(set=nullItem, key=x.key, rank=x.rank + 1, left=x, right=y, next=null)
    z.set = nullItem
    defill(z)
    return z

########
# meld #
########


def meld(H1: Node, H2: Node) -> Node:
    return key_swap(meldable_meld(rank_swap(H1), rank_swap(H2)))

#################
# meldable_meld #
#################


def meldable_meld(H1: Node, H2: Node) -> Node:
    if H1.rank > H2.rank:
        H1, H2 = H2, H1
    if H2 == null:
        return H1
    else:
        return meldable_insert(H1, meldable_meld(rank_swap(H1.next), H2))

#################################################################


def randlist(n) -> List[float]:
    return [random.random() for i in range(n)]


def randperm(n) -> List[float]:
    return random.sample(list(range(n)), n)


def build(lst) -> Node:
    P = make_heap()
    for it in lst:
        P = insert(Item(it), P)
    return P


def extract(P: Node) -> List[float]:
    lst = []
    while P != null:
        lst.append(find_min(P)[0].key)
        P = delete_min(P)
    return lst

BUG = Exception

def sort(lst: List[float]) -> List[float]:
    print(lst)
    P = build(lst)
    lst1 = extract(P)
    if T == INF:
        for i in range(1, len(lst)):
            if lst1[i] < lst1[i-1]:
                raise BUG("BUG!!!")
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
