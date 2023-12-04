source("strutils.R")

library(purrr)
library(readr)

input <- readLines("test-inputs/03-test.txt")
input <- do.call(rbind, map(input, function(x) strsplit(x, "")[[1]]))

get_number <- function(v, pos) {
  while (pos > 0 && is.digit(v[[pos]])) {
    pos <- pos - 1
  }
  pos <- pos + 1
  res <- list()
  while (pos <= length(v) && grepl("[0-9]", v[[pos]])) {
    res <- append(res, v[[pos]])
    pos <- pos + 1
  }
  res <- do.call(paste, c(res, list(sep = "")))
  return(parse_integer(res))
}

contains <- function(l, num) {
  for (elem in l) {
    if (all(num == elem)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

moore_neighbors <- function(pos) {
  res <- list()
  res[[length(res) + 1]] <- c(row = pos[['row']] - 1, col = pos[['col']])
  res[[length(res) + 1]] <- c(row = pos[['row']] - 1, col = pos[['col']] + 1)
  res[[length(res) + 1]] <- c(row = pos[['row']], col = pos[['col']] + 1)
  res[[length(res) + 1]] <- c(row = pos[['row']] + 1, col = pos[['col']] + 1)
  res[[length(res) + 1]] <- c(row = pos[['row']] + 1, col = pos[['col']])
  res[[length(res) + 1]] <- c(row = pos[['row']] + 1, col = pos[['col']] - 1)
  res[[length(res) + 1]] <- c(row = pos[['row']], col = pos[['col']] - 1)
  res[[length(res) + 1]] <- c(row = pos[['row']] - 1, col = pos[['col']] - 1)
  return(res)
}

symbols <- list()
for (i in 1:dim(input)[[1]]) {
  coord <- grep("[^\\. | 0-9]", input[i,])
  if (!identical(coord, integer(0))) {
    for (c in coord) {
      symbols[[length(symbols) + 1]] <- c(row = i, col = c)
    }
  } 
}

find_part_numbers <- function() {
  part_numbers <- list()
  for (s in symbols) {
    neighbors <- moore_neighbors(s)
    for (n in neighbors) {
      row <- n[['row']]
      col <- n[['col']]
      if (is.digit(input[row, col])) {
        part_number <- c(s, get_number(input[row,], col))
        if (!contains(part_numbers, part_number)) {
          part_numbers[[length(part_numbers) + 1]] <- part_number
        }
      }
    }
  }
  return(part_numbers)
}

find_gear_ratios <- function() {
  res <- 0
  for (s in symbols) {
    if (input[s[['row']], s[['col']]] == "*") {
      part_numbers <- list()
      neighbors <- moore_neighbors(s)
      for (n in neighbors) {
        row <- n[['row']]
        col <- n[['col']]
        if (is.digit(input[row, col])) {
          part_number <- c(s, get_number(input[row,], col))
          if (!contains(part_numbers, part_number)) {
            part_numbers[[length(part_numbers) + 1]] <- part_number
          }
        }
      }
      if (length(part_numbers) == 2) {
        res <- res + (part_numbers[[1]] * part_numbers[[2]])
      }
    }
  }
  return(res)
}

part1 <- Reduce("+", find_part_numbers(), c(x = 0, y = 0, 0))[[3]]
part2 <- find_gear_ratios()[[3]]

print(part1)
print(part2)
