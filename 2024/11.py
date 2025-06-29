import sys

if len(sys.argv) != 2:
    print(f"Usage: python3 {sys.argv[0]} <file>")
    sys.exit(1)


def expand_stone(stone, cache, iterations):
    if iterations == 0:
        return 1

    if (stone, iterations) in cache:
        return cache[(stone, iterations)]

    if stone == 0:
        cache[(stone, iterations)] = expand_stone(1, cache, iterations - 1)
    elif len(str(stone)) % 2 == 1:
        cache[(stone, iterations)] = expand_stone(stone * 2024, cache,
                                                  iterations - 1)
    else:
        s = str(stone)
        mid = len(s) // 2
        cache[(stone, iterations)] = nstones([int(s[:mid]), int(s[mid:])],
                                             cache, iterations - 1)
    return cache[(stone, iterations)]


def nstones(stones, cache, iterations):
    if not stones:
        return 0

    return (expand_stone(stones[0], cache, iterations) +
            nstones(stones[1:], cache, iterations))


with open(sys.argv[1], "r") as f:
    stones = [int(x) for line in f for x in line.strip().split()]

# Part 1
print(nstones(stones, {}, 25))

# Part 2
print(nstones(stones, {}, 75))
