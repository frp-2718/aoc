from collections import deque
import sys

sys.setrecursionlimit(100000)


def checksum(disk):
    checksum = 0
    pos = 0
    for block in disk:
        for i in range(block[0]):
            block_id = block[1] if block[1] > 0 else 0
            checksum += (i + pos) * block_id
        pos += block[0]
    return checksum


def defrag_block(disk, reordered):
    current_block = disk[0]

    # Data block
    if current_block[1] >= 0:
        reordered.append(disk.popleft())
        return disk, reordered

    # Space block
    candidate = disk[-1]
    reordered.append((min(current_block[0], candidate[0]), candidate[1]))
    disk.popleft()
    disk.pop()
    diff = current_block[0] - candidate[0]
    if diff > 0:
        if disk[-1][1] == -1:
            disk.pop()
        disk.appendleft((diff, -1))
    else:
        disk.append((candidate[0] - current_block[0], candidate[1]))

    return disk, reordered


def find_space(disk):
    for i, block in enumerate(disk):
        if block[1] == -1 and block[0] >= disk[-1][0]:
            return i
    return -1


def defrag_file(disk, reordered):
    if disk[-1][1] == -1:
        reordered.appendleft(disk.pop())
        return disk, reordered

    fit = find_space(disk)
    if fit == -1:
        block = disk.pop()
        reordered.appendleft(block)
    else:
        diff = disk[fit][0] - disk[-1][0]
        disk[fit] = disk.pop()
        disk.append((disk[fit][0], -1))
        if diff > 0:
            new_space = (diff, -1)
            new_disk = list(disk)
            new_disk.insert(fit + 1, new_space)
            disk = deque(new_disk)

    return disk, reordered


def defrag(disk, reordered, f):
    if len(disk) == 0:
        return reordered
    return defrag(*f(disk, reordered), f)


if len(sys.argv) != 2:
    print(f"Usage: python3 {sys.argv[0]} <file>")
    sys.exit(1)

with open(sys.argv[1], "r") as f:
    disk_map = f.read().strip()
    blocks = [int(c) for c in disk_map]

disk = []

for i, block in enumerate(blocks):
    if i % 2 == 0:
        disk.append((block, i//2))
    elif block != 0:
        disk.append((block, -1))
    block -= 1

disk1 = deque(disk)
disk2 = deque(disk1)

# Part 1
reordered_blocks = defrag(disk1, deque([]), defrag_block)
print(checksum(reordered_blocks))

# Part 2
reordered_files = defrag(disk2, deque([]), defrag_file)
print(checksum(reordered_files))
