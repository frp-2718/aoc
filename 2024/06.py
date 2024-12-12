from copy import deepcopy


def path(pos, grid):
    step = (-1, 0)
    locations = set()
    visited = set()
    visited.add(pos)
    locations.add((step, pos))
    while (pos[0] > 0 and pos[0] < len(grid)-1
           and pos[1] > 0 and pos[1] < len(grid[0])-1):
        next_pos = tuple(x + y for x, y in zip(pos, step))
        if (step, next_pos) in locations:
            return []
        if grid[next_pos[0]][next_pos[1]] == "#":
            if step[0] == -1:
                step = (0, 1)
            elif step[0] == 0 and step[1] == 1:
                step = (1, 0)
            elif step[0] == 1:
                step = (0, -1)
            else:
                step = (-1, 0)
        else:
            pos = next_pos
            visited.add(pos)
            locations.add((step, pos))
    return list(visited)


grid = []
start = ()
with open("test-inputs/06-test.txt", "r") as file:
    for line in file:
        grid.append(list(line.strip()))

for row, line in enumerate(grid):
    try:
        start = (row, line.index('^'))
    except ValueError:
        continue

visited = path(start, grid)

# part 1
print(len(visited))

total = 0
for pos in visited:
    new_grid = deepcopy(grid)
    new_grid[pos[0]][pos[1]] = '#'
    if len(path(start, new_grid)) == 0:
        total += 1

# part 2
print(total)
