import sys

if len(sys.argv) != 2:
    print(f"Usage: python3 {sys.argv[0]} <file>")
    sys.exit(1)


# part 1
def diagonals(s):
    diag1 = ["" for _ in range(len(s) + len(s[0]) - 1)]
    diag2 = diag1[:]
    for row, string in enumerate(s):
        for col, char in enumerate(string):
            diag1[row - col + len(s) - 1] += char
            diag2[row + col] += char
    return (diag1, diag2)


with open(sys.argv[1], "r") as file:
    h_strings = [line.strip() for line in file]

vertical_strings = ["".join(col) for col in zip(*h_strings)]
diag1_strings, diag2_strings = diagonals(h_strings)

strings = h_strings + vertical_strings + diag1_strings + diag2_strings
strings += [s[::-1] for s in strings]

total = 0
for s in strings:
    total += s.count("XMAS")

print(total)

# part 2
total = 0
for i in range(len(h_strings)-2):
    for j in range(len(h_strings[0])-2):
        if h_strings[i+1][j+1] == "A":
            corners = (h_strings[i][j] +
                       h_strings[i+2][j] +
                       h_strings[i][j+2] +
                       h_strings[i+2][j+2])
            if corners in ["MMSS", "MSMS", "SSMM", "SMSM"]:
                total += 1

print(total)
