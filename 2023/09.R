library(readr)

input <- readLines("test-inputs/09-test.txt")

sequences <- strsplit(input, "[ ]+") |>
  lapply(parse_number)

generate_sequence <- function(s) {
  res <- c()
  for (i in 2:length(s)) {
    res <- append(res, s[[i]] - s[[i - 1]])
  }
  return(res)
}

predict <- function(s) {
  lasts <- c()
  while (!all(s == 0)) {
    lasts <- append(lasts, s[[length(s)]])
    s <- generate_sequence(s)
  }
  return(sum(lasts))
}

part1 <- 0
part2 <- 0

for (s in sequences) {
  part1 <- part1 + predict(s)
  part2 <- part2 + predict(rev(s))
}

print(part1)
print(part2)
