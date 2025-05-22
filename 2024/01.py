import sys

if len(sys.argv) != 2:
    print(f"Usage: python3 {sys.argv[0]} <file>")
    sys.exit(1)

l1 = []
l2 = []

with open(sys.argv[1], "r") as file:
    for line in file:
        values = line.split()
        l1.append(int(values[0]))
        l2.append(int(values[1]))

l1.sort()
l2.sort()

distance = 0
similarity = 0

for val1, val2 in zip(l1, l2):
    distance += abs(val1 - val2)
    similarity += val1 * l2.count(val1)

print(distance)
print(similarity)
