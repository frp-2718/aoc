source("commonutils.R")

library(purrr)
library(readr)

input <- readLines("test-inputs/05-test.txt")

# Input parsing

seeds <- (strsplit(input[[1]], ": ")[[1]][[2]] |>
  strsplit(" ") |>
  map(parse_double))[[1]]

almanac <- Filter(function(x) nchar(x) > 0, input[-c(1)])
almanac[[length(almanac) + 1]] <- "s"

layers <- list()
layer <- double(0)
for (l in almanac) {
  if (!is.digit(substr(l, 1, 1))) {
    if (length(layer) > 0) {
      layers[[length(layers) + 1]] <- layer
    }
    layer <- list()
  } else {
    newlayer <- (map(strsplit(l, " "), parse_double))[[1]]
    layer[[length(layer) + 1]] <- newlayer
  }
}

# Part 1

link <- function(seed, layer) {
  for (v in layer) {
    if (seed >= v[[2]] && seed <= v[[2]] + v[[3]]) {
      return(v[[1]] + seed - v[[2]])
    } 
  }
  return(seed)
}

find_locations <- function(seeds, layers) {
  locations <- integer(0)
  for (seed in seeds) {
    location <- seed
    for (layer in layers) {
      location <- link(location, layer)
    }
    locations <- append(locations, location)
  }
  return(locations)
}

part1 <- min(find_locations(seeds, layers))

# Part 2

seeds_to_ranges <- function(seeds) {
  ranges <- list()
  for (i in seq(1, length(seeds), by = 2)) {
    ranges[[length(ranges) + 1]] <- c(seeds[[i]], seeds[[i]] + seeds[[i + 1]] - 1)
  }
  return(ranges)
}

map_range <- function(range, mapping) {
  low <- range[[1]]
  up <- range[[2]]
  low_src <- mapping[[2]]
  up_src <- low_src + mapping[[3]] - 1
  low_dst <- mapping[[1]]

  res <- list()
  
  mapped <- function(n) {
    return(n + (low_dst - low_src))
  }
  
  if (up < low_src || low > up_src) {
    res[[length(res) + 1]] <- range
  } else {
    if (low < low_src && up < up_src) {
      res[[length(res) + 1]] <- c(mapped(low_src), mapped(up))
      res[[length(res) + 1]] <- c(low, low_src-1)
    } else if (low > low_src && up > up_src) {
      res[[length(res) + 1]] <- c(mapped(low), mapped(up_src))
      res[[length(res) + 1]] <- c(up_src + 1, up)
    } else if (low < low_src && up > up_src) {
      res[[length(res) + 1]] <- c(low, low_src - 1)
      res[[length(res) + 1]] <- c(mapped(low_src), mapped(up_src))
      res[[length(res) + 1]] <- c(up_src + 1, up)
    } else {
      res[[length(res) + 1]] <- c(mapped(low), mapped(up))
    }
  }
  return(res)
}

map_range_layer <- function(r, layer) {
  for (mapping in layer) {
    res <- map_range(r, mapping)
    if (length(res) > 1 || !all(res[[1]] == r)) {
      return(res)
    }
  }
  return(list(r))
}

map_ranges <- function(ranges, layer) {
  res <- list()
  for (r in ranges) {
    res <- c(res, map_range_layer(r, layer))
  }
  return(res)
}

concat_lists <- function(l1, l2) {
  for (item in l2) {
    l1[[length(l1) + 1]] <- item
  }
  return(l1)
}

find_all_locations <- function(ranges, layers) {
  res <- map_ranges(ranges, layers[[1]])
  for (layer in layers[-c(1)]) {
    res <- map_ranges(res, layer)
  }
  return (res)
}

part2 <- find_all_locations(seeds_to_ranges(seeds), layers) |>
  map(function(x) x[[1]]) |>
  unlist() |>
  min()

print(part1)
print(part2)
