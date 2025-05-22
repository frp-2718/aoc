import math
import sys

if len(sys.argv) != 2:
    print(f"Usage: python3 {sys.argv[0]} <file>")
    sys.exit(1)


def concat_int(a, b):
    digits = math.floor(math.log10(a) + 1)
    return b * 10**digits + a


def solve(numbers, part2=False):
    if len(numbers) == 1:
        return numbers
    plus_result = [numbers[0] + n for n in solve(numbers[1:], part2)]
    mult_result = [numbers[0] * n for n in solve(numbers[1:], part2)]
    if part2:
        concat_result = [concat_int(numbers[0], n) for n in solve(numbers[1:], part2)]
        return plus_result + mult_result + concat_result
    return plus_result + mult_result


def calibration(equations, part2=False):
    calibration = 0
    for equation in equations:
        total, numbers = equation
        results = solve(numbers[::-1], part2)
        for result in results:
            if result == total:
                calibration += result
                break
    return calibration


equations = []
with open(sys.argv[1], "r") as file:
    for line in file:
        total, numbers = line.split(": ")
        equations.append((int(total), [int(n) for n in numbers.split()]))

# part 1
print(calibration(equations, False))

# part 2
print(calibration(equations, True))
