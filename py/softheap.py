# A python implementation of the soft heaps described in the paper
# "Soft Heaps Simplified", by Haim Kaplan, Robert E. Tarjan and Uri Zwick.
# (c) Haim Kaplan, Robert E. Tarjan and Uri Zwick.

import random
from dataclasses import dataclass
import dataclasses as d
import math
from math import inf
from typing import Union, Tuple, List, Iterator
from copy import copy
T = 2
# T = 20

########
# Item #
########

debug = False


@d.dataclass()
class Item:
    key: Union[float, int]
    next: 'Item' = None

    def __post_init__(self):
        if self.next is None:
            self.next = self

    def size(self):
        cur = self
        count = 0
        while True:
            if debug:
                print(f"Item.size: {cur.key}")
            if cur is None or cur is nullItem:
                raise Exception("Item.size(): cur is None or cur is nullItem")
                # return count
            count += 1
            cur = cur.next
            if cur is self or cur is None or cur is nullItem:
                if debug:
                    print(f"Item.size: {count}")
                return count

    def items(self):
        cur = self
        while True:
            yield cur.key
            cur = cur.next
            if cur is self or cur is None or cur is nullItem:
                return

nullItem = Item(inf)

########
# Node #
########

@dataclass()
class Node:
    set: Item = d.field(default_factory=lambda : nullItem)
    key: float = inf
    rank: Union[float,int] = inf
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
    
    def items1(self):
        # print("foo")
        global debug
        # if debug:
        #     print(f"Node.items1 key: {self.key}")
        yield from self.set.items()
        if self.left is not None and self.left is not null and self.left is not self:
            yield from self.left.items1()
        if self.right is not None and self.right is not null and self.right is not self:
            yield from self.right.items1()

    def size1(self):
        if debug:
            print(f"Node.size1(): key: {self.key}")
            items = list(self.items1())
            print (f"size1: {self.set.size()}\t{items}")
        count = self.set.size()
        for child in (self.left, self.right):
            if child is not None and child is not null and child is not self:
                count += child.size1()
        return count
        # return (self.set.size() +
        #         (0 if self.left is None or self.left is null or self.left is self else self.left.size1()) +
        #         (0 if self.right is None or self.right is null or self.right is self else self.right.size1()))


    def items(self):
        cur = self
        while True:
            yield from cur.items1()
            cur = cur.next
            if cur is self or cur is None or cur is null:
                return

    def size(self):
        if debug:
            print(f"Node.size() ###############################")
        cur = self
        count = 0
        while True:
            count += self.size1()
            cur = cur.next
            if cur is self or cur is None or cur is null:
                return count

null = Node(rank=inf)
# null.left = null
# null.right = null
# null.set = nullItem
# null.key = inf
# null.rank = inf
# null.left = null
# null.right = null
# null.next = null

def get_all_uncorrupted(P: Union[None, Node]) -> List[float]:
    # TODO: this needs to take next into account.
    def helper(x: Union[None, Node]) -> Iterator[float]:
        if x is None or x is null:
            return
        else:
            yield x.key
            yield from helper(x.left)
            yield from helper(x.right)
    cur = P
    so_far = []
    while cur is not None and cur is not null:
        # print("moving to next node")
        so_far.extend(list(helper(cur)))
        cur = cur.next
        if cur is P:
            break
    return so_far

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


def insert(e: Union[float, int], H: Node) -> Node:
    return key_swap(meldable_insert(make_root(Item(e)), rank_swap(H)))

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


def build(lst: List[Union[float, int]]) -> Node:
    global debug, T
    debug = False
    T = 2
    P = make_heap()
    s = 0
    so_far = []
    for it in lst:
        s += 1
        so_far.append(it)
        P = insert(it, P)
        # hmm, P.size is buggy, maybe?.  Go with items.
        items = list(P.items())
        assert len(so_far) == len(items), f"{so_far} != {items}"
        uncorrupted = get_all_uncorrupted(P)
        assert len(uncorrupted) <= len(items), f"{uncorrupted} > {items}"
        assert set(uncorrupted).issubset(set(items)), f"{uncorrupted} not subset of {items}"
        print(f"{len(so_far)}: {len(uncorrupted)}\t{uncorrupted}")
        # if P.size() != s:
        #     msg = f"{P.size()} != {s}, {list(P.items())} {so_far}"
        #     debug = True
        #     # list(P.items())
        #     # debug = False
        #     assert P.size() == s, msg
        #     return P
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
    corruptions = 0
    for i in range(1, len(lst)):
        if lst1[i] < lst1[i-1]:
            if T == inf:
                raise BUG("BUG!!!")
            else:
                corruptions += 1
    print(f"corruptions: {corruptions}")
    print(lst1)
    print(" ")
    return lst1

def sort1(lst: List[float]) -> List[float]:
    global T
    T = inf
    P = build(lst)
    print(get_all_uncorrupted(P))
    print(P.size())
    for _ in range(len(lst)//2):
        P = delete_min(P)
    print(get_all_uncorrupted(P))
    print(P.size())

def from_upstream():
    sort(randperm(100))

    T = 3
    sort(randperm(100))

    # T = inf
    P = build(randperm(100))
    Q = build(randperm(200))
    print(extract(meld(P, Q)))

build(range(1000))
