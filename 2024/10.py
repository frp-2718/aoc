import sys


def valid_neighbors(grid, node):
    for drow, dcol in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        nrow, ncol = node[0] + drow, node[1] + dcol
        if (0 <= nrow < len(grid) and 0 <= ncol < len(grid[0])
                and grid[nrow][ncol] - grid[node[0]][node[1]] == 1):
            yield (nrow, ncol)


def explore(grid, node, visited, distinct):
    score = 0
    if grid[node[0]][node[1]] == 9:
        score += 1
    visited.add(node)
    for n in valid_neighbors(grid, node):
        if n not in visited or distinct:
            score += explore(grid, n, visited, distinct)
    return score


def find_trails(grid, distinct):
    total = 0
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if grid[row][col] == 0:
                total += explore(grid, (row, col), set(), distinct)
    return total


if len(sys.argv) != 2:
    print(f"Usage: python3 {sys.argv[0]} <file>")
    sys.exit(1)

with open(sys.argv[1], "r") as f:
    grid = [[int(c) for c in line.strip()] for line in f]

# Part 1
print(find_trails(grid, False))

# Part 2
print(find_trails(grid, True))
