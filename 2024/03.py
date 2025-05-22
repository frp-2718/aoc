import re
import sys

if len(sys.argv) != 2:
    print(f"Usage: python3 {sys.argv[0]} <file>")
    sys.exit(1)


def sum_of_mults(line):
    pattern = r"mul\((\d+),(\d+)\)"
    instructions = re.findall(pattern, line)
    return sum([int(a) * int(b) for a, b in instructions])


result_2 = 0

with open(sys.argv[1], "r") as file:
    content = "do()" + file.read()
    # part 1
    result_1 = sum_of_mults(content)
    # part 2
    segments = content.split("don't()")
    for segment in segments:
        for dos in segment.split("do()")[1:]:
            result_2 += sum_of_mults(dos)

print(result_1)
print(result_2)
