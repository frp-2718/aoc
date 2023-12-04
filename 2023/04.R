library(purrr)
library(readr)

input <- readLines("test-inputs/04-test.txt")

matching_numbers <- function(p) {
  matches <- p[[1]] %in% p[[2]]
  return(length(matches[matches == TRUE]))
}

res <- input |>
  map(function (x) strsplit(x, ":\\D+")[[1]][[2]]) |>
  as.character() |>
  strsplit("\\D+\\|\\D+") |>
  map(function (x) strsplit(x, "\\D+")) |>
  map(function (x) map(x, parse_integer)) |>
  map(matching_numbers)

instances <- map(res, function (x) c(copies = 1, matches = x))

for (i in 1:length(instances)) {
  ncopies <- instances[[i]][['copies']]
  for (j in 1:ncopies) {
    nmatches <- instances[[i]][['matches']]
    if (nmatches > 0) {
      for (k in 1:nmatches) {
        instances[[i + k]][['copies']] <- instances[[i + k]][['copies']] + 1
      }
    }
  }
}

part1 <- Reduce("+",  map(res, function (x) ifelse(x == 0, 0, 2^(x - 1))), 0)
part2 <- Reduce("+", map(instances, function (x) x[['copies']]), 0)

print(part1)
print(part2)