# needs hypothesis and sortedcontainers, pytest
import dataclasses as d
from collections import Counter

import hypothesis.strategies as st
from hypothesis import (HealthCheck, Phase, Verbosity, assume, event, example,
                        find, given, note, reject, reproduce_failure, seed,
                        settings)
from hypothesis.stateful import (RuleBasedStateMachine, invariant,
                                 precondition, rule)
  
from typing import List, Union, Any

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

def convert_to_single_restrict_up(commands: List[Union[Push, RestrictUp]]) -> List[Union[Push, RestrictUp]]:
  """Remove multiple repeated RestrictUp commands by shifting them around."""
  l = []
  actual_size = 0
  max_size = 0
  for command in commands:
    match command:
      case Push(value):
        if actual_size < max_size:
          l.append(RestrictUp())
          actual_size += 1
        l.append(Push(value))
      case RestrictUp():
        max_size += 1
  return l

r_up = st.lists(st.one_of(st.just(RestrictUp()), st.integers().map(Push)))

def back_to_pop(commands: List[Union[Push, RestrictUp]]) -> List[Union[Push, PopMin]]:
  """Convert a heap with a restriction on its size to a heap."""
  l = []
  act_size = 0
  max_size = 0
  for command in commands:
    match command:
      case Push(value):
        l.append(Push(value))
        if act_size < max_size:
          act_size += 1
        else:
          l.append(PopMin())
      case RestrictUp():
        max_size += 1
  return l

def invert(commands: List[Union[Push, RestrictUp]]) -> List[Union[Push, RestrictUp]]:
  """Invert the commands.
  This is the dual in the nested matroid sense."""
  l: List[Union[Push, PopMin]] = []

  for command in commands[::-1]:
    match command:
      case Push(value):
        l.append(Push(-value))
      case RestrictUp():
        l.append(PopMin())
  return convert_to_single_restrict_up(convert_pop_min_to_restrict_up(l))

def get_all_pushes(commands: List[Union[Push, Any]]) -> List[int]:
  """Get all the pushes."""
  return [command.value for command in commands if isinstance(command, Push)]

@given(r_up)
def test_invert(commands: List[Union[Push, RestrictUp]]):
  """Test that invert works."""
  all_pushes = [command.value for command in commands if isinstance(command, Push)]
  direct = simulate_push_restrict_up(commands)
  inv = invert(commands)
  note(f"inv: {inv}")
  indirect = [-x for x in simulate_push_restrict_up(inv)]
  note(f"direct: {direct}")
  note(f"indirect: {indirect}")
  assert Counter(all_pushes) == Counter(direct) + Counter(indirect)

@given(r_up)
def test_convert_to_single_restrict_up(commands: List[Union[Push, RestrictUp]]):
  """Test that convert_to_single_restrict_up works."""
  direct = simulate_push_restrict_up(commands)
  conv = convert_to_single_restrict_up(commands)
  indirect = simulate_push_restrict_up(conv)
  assert direct == indirect

  # assert that we don't have two consecutive RestrictUp commands
  for command, next_command in zip(conv, conv[1:]):
    match command, next_command:
      case RestrictUp(), RestrictUp():
        assert False

def fiddle():
  if False:
    c0 = r_up.example()
    c1 = convert_to_single_restrict_up(c0)
    print(c1)

  cx = pops_and_pushes.example()
  print(cx)
  cy = convert_pop_min_to_restrict_up(cx)
  print(cy)
  cz = convert_to_single_restrict_up(cy)
  print(cz)


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

pops_and_pushes = st.lists(st.one_of(st.just(PopMin()), st.integers().map(Push)))

@given(pops_and_pushes)
def test_convert_pop_min_to_restrict_up(commands: List[Union[Push, PopMin]]):
  direct = simulate_insert_pop(commands)
  indirect = simulate_push_restrict_up(convert_pop_min_to_restrict_up(commands))
  assert direct == indirect

@given(pops_and_pushes)
def test_convert_pop_min_to_restrict_up_and_back(commands: List[Union[Push, PopMin]]):
  direct = simulate_insert_pop(commands)
  indirect = simulate_insert_pop(back_to_pop(convert_pop_min_to_restrict_up(commands)))
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

if __name__ == '__main__':
  fiddle()
