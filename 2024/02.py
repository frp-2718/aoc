def safe(report, tolerance):
    if len(report) == 1:
        return True
    if (report[1] - report[0]) < 0:
        report.reverse()
    for i in range(1, len(report)):
        diff = report[i] - report[i-1]
        if diff < 1 or diff > 3:
            if not tolerance:
                return False
            else:
                return safe_tolerance(report)
    return True


def safe_tolerance(report):
    for i in range(len(report)):
        r = report[:i] + report[i+1:]
        if safe(r, False):
            return True
    return False


nsafe = 0
nsafe_tolerance = 0

with open("test-inputs/02-test.txt", "r") as file:
    for line in file:
        report = list(map(int, line.split()))
        if safe(report, False):
            nsafe += 1
        if safe(report, True):
            nsafe_tolerance += 1

print(nsafe)
print(nsafe_tolerance)
