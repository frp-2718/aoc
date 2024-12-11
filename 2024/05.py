from functools import cmp_to_key


def compare(a, b):
    if a in rules and b in rules[a]:
        return 1
    elif a in rules[b]:
        return -1


def ordered(update):
    for i in range(len(update)-1):
        if compare(update[i], update[i+1]) < 0:
            return 0
    return update[len(update) // 2]


rules = {}
updates = []

with open("test-inputs/05-test.txt", "r") as file:
    for line in file:
        if "|" in line:
            before, after = line.split("|")
            rules.setdefault(before, []).append(after.strip())
        elif "," in line:
            updates.append(line.strip().split(","))

total_correct = 0
total_incorrect = 0

for update in updates:
    correct = int(ordered(update))
    if correct:
        total_correct += correct
    else:
        update.sort(key=cmp_to_key(compare))
        total_incorrect += int(update[len(update) // 2])

print(total_correct)
print(total_incorrect)
