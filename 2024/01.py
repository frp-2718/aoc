l1 = []
l2 = []

with open("test-inputs/01-test.txt", "r") as file:
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
