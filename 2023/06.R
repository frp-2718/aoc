library(purrr)
library(readr)
library(stringr)

input <- readLines("test-inputs/06-test.txt")

races <- map(str_extract_all(input, "[0-9]+"), parse_integer)

solve_quadratic <- function(a, b, c) {
  delta <- as.double(b) * as.double(b) - 4 * a * as.double(c)
  if (delta < 0) {
    return(NULL)
  } else {
    return(c(((-b + sqrt(delta)) / (2 * a)), ((-b - sqrt(delta)) / (2 * a))))
  }
}

num_winning_strategies <- function(t, r) {
  sol <- sort(solve_quadratic(-1, t, -r))
  return(max(0, ceiling(sol[[2]]) - floor(sol[[1]]) - 1))
}

total <- 1
for (i in 1:length(races[[1]])) {
  total <- total * num_winning_strategies(races[[1]][[i]], races[[2]][[i]])
}

part1 <- total

new_race <- map(input, function(x)
  parse_double(strsplit(str_replace_all(x, " +", ""), ":")[[1]][[2]]))

part2 <- num_winning_strategies(new_race[[1]], new_race[[2]])

print(part1)
print(part2)
