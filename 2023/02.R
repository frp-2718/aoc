library(purrr)
library(readr)
library(stringr)

input <- readLines("test-inputs/02-test.txt")

records <- strsplit(input, "; ")

record_to_triple <- function(record) {
  res <- c(
    parse_number(str_extract(record, "[0-9]* red")),
    parse_number(str_extract(record, "[0-9]* green")),
    parse_number(str_extract(record, "[0-9]* blue"))
  )
  return(res)
}

games <- map(records, function(r) map(r, record_to_triple))

possible <- function(games) {
  bag <- c(12, 13, 14)
  res <- games |>
    map(function(g) map(g , function(x) x <= bag)) |>
    map(function(l) map(l, all)) |>
    map(function(x) all(as.logical(x)))
  return(res)
}

sum_possible <- function(p) {
  total <- 0
  for(i in 1:length(p)) {
    if (p[[i]] | is.na(p[[i]])) {
      total <- total + i
    }
  }
  return(total)
}

sum_power <- function(games) {
  total <- 0
  for (game in games) {
    total <- total + Reduce("*", do.call(pmax, c(game, list(na.rm = TRUE))), 1) 
  }
  return(total)
}

part1 <- sum_possible(possible(games))
part2 <- sum_power(games)

print(part1)
print(part2)
