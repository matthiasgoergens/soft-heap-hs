# needs hypothesis and sortedcontainers, pytest
import dataclasses as d

import hypothesis.strategies as st
from hypothesis import (HealthCheck, Phase, Verbosity, assume, event, example,
                        find, given, note, reject, reproduce_failure, seed,
                        settings)
from hypothesis.stateful import (RuleBasedStateMachine, invariant,
                                 precondition, rule)
  
from typing import List, Union

from sortedcontainers import SortedList
from itertools import accumulate

@d.dataclass()
class PopMin:
  """A command to remove the minimum element from a heap."""
  pass

@d.dataclass()
class PopMax:
  """A command to remove the maximum element from a heap."""
  pass

@d.dataclass()
class Push:
  """A command to add an element to a heap."""
  value: int

@d.dataclass()
class Restrict:
  """A command to restrict the size of a heap."""
  limit: int

@d.dataclass()
class RestrictUp:
  """A command to increase the maximum size of a heap."""
  pass

def convert_pop_min_to_restrict_up(commands: List[Union[Push, PopMin]]) -> List[Union[Push, RestrictUp]]:
  """Convert a heap to a heap with a restriction on its size."""
  l = []
  size = 0

  sizes = []
  for command in commands:
    match command:
      case Push(value):
        size += 1
        sizes.append(size)
      case PopMin():
        size = max(0, size - 1)
        sizes.append(size)
  sizes_min = list(accumulate(sizes[::-1], min))[::-1]

  heap_size = 0
  for (size_max, command) in zip(sizes_min, commands):
    match command:
      case Push(value):
        while heap_size < size_max:
          heap_size += 1
          l.append(RestrictUp())
        l.append(Push(value))
  return l

@given(st.lists(st.one_of(st.just(PopMin()), st.integers().map(Push))))
def test_convert_pop_min_to_restrict_up(commands: List[Union[Push, PopMin]]):
  direct = simulate_insert_pop(commands)
  indirect = simulate_push_restrict_up(convert_pop_min_to_restrict_up(commands))
  assert direct == indirect

def simulate_insert_pop(heap: List[Union[Push, PopMin, PopMax]]) -> List[int]:
  """Remove the minimum element from a heap."""
  l = SortedList()
  for command in heap:
    match command:
      case Push(value):
        l.add(value)
      case PopMin():
        if len(l) > 0:
          l.pop(0)
      case PopMax():
        if len(l) > 0:
          l.pop(-1)
  return list(l)

def simulate_push_restrict_up(heap: List[Union[Push, RestrictUp]]) -> List[int]:
  l = SortedList()
  size = 0
  for command in heap:
    match command:
      case Push(value):
        l.add(value)
        while len(l) > size:
          l.pop(0)
      case RestrictUp():
        size += 1
  return list(l)

@given(st.lists(st.one_of(st.just(PopMin()), st.just(PopMax()), st.integers().map(Push))))
def test_simulate_insert_pop(commands: List[Union[Push, PopMin, PopMax]]):
  """Test that simulate_insert_pop works."""
  simulate_insert_pop(commands)

@given(st.lists(st.one_of(st.just(RestrictUp()), st.integers().map(Push))))
def test_simulate_push_restrict_up(commands):
  """Test that simulate_push_restrict_up works."""
  simulate_push_restrict_up(commands)
