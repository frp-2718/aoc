from collections import defaultdict
from itertools import combinations
import sys

if len(sys.argv) != 2:
    print(f"Usage: python3 {sys.argv[0]} <file>")
    sys.exit(1)


def add_antinodes_m1(a1, a2, antinodes):
    v = tuple(x - y for x, y in zip(a1, a2))
    antinode1 = tuple(x + y for x, y in zip(a1, v))
    antinode2 = tuple(x - y for x, y in zip(a2, v))
    if in_bounds(antinode1):
        antinodes.add(antinode1)
    if in_bounds(antinode2):
        antinodes.add(antinode2)


def add_antinodes_m2(a1, a2, antinodes):
    v = tuple(x - y for x, y in zip(a1, a2))

    while in_bounds(a1):
        antinodes.add(a1)
        a1 = tuple(x + y for x, y in zip(a1, v))

    while in_bounds(a2):
        antinodes.add(a2)
        a2 = tuple(x - y for x, y in zip(a2, v))


def in_bounds(antinode):
    row, col = antinode[0], antinode[1]
    return (row >= 0 and col >= 0 and col <= len(lines[0]) - 1
            and row <= len(lines) - 1)


with open(sys.argv[1], "r") as f:
    lines = f.read().splitlines()

antennas = defaultdict(list)
antinodes_1 = set()
antinodes_2 = set()

for row, line in enumerate(lines):
    for col, char in enumerate(line):
        if char != '.':
            antennas[char].append((row, col))

couples = []
for key, group in antennas.items():
    c = list(combinations(group, 2))
    couples.extend(c)

# Part 1
for couple in couples:
    add_antinodes_m1(couple[0], couple[1], antinodes_1)

# Part 2
for couple in couples:
    add_antinodes_m2(couple[0], couple[1], antinodes_2)

print(len(antinodes_1))
print(len(antinodes_2))
