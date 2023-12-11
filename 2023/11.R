input <- readLines("test-inputs/11-test.txt")
grid <- do.call(rbind, strsplit(input, ""))
galaxies <- which(grid == "#", arr.ind = TRUE)
empty_rows <- setdiff(1:(dim(grid)[[1]]), galaxies[, "row"])
empty_cols <- setdiff(1:(dim(grid)[[2]]), galaxies[, "col"])

manhattan <- function(a, b) {
  return(abs(a[["row"]] - b[["row"]]) + abs(a[["col"]] - b[["col"]]))
}

expanded_manhattan <- function(a, b, expansion) {
  min_row <- min(a[["row"]], b[["row"]])
  max_row <- max(a[["row"]], b[["row"]])
  min_col <- min(a[["col"]], b[["col"]])
  max_col <- max(a[["col"]], b[["col"]])
  
  nrows <- length(empty_rows[empty_rows %in% min_row:max_row])
  ncols <- length(empty_cols[empty_cols %in% min_col:max_col])
  
  return(manhattan(a, b) + (expansion - 1) * (nrows + ncols))
}

sum_dist <- function(expansion) {
  total <- 0
  for (i in 1:dim(galaxies)[[1]]) {
    for (j in i:dim(galaxies)[[1]]) {
      g1 <- c(galaxies[i, "row"], galaxies[i, "col"])
      g2 <- c(galaxies[j, "row"], galaxies[j, "col"])
      total <- total + expanded_manhattan(g1, g2, expansion)
    }
  }
  return(total)
}

part1 <- sum_dist(2)
part2 <- sum_dist(1000000)

print(part1)
print(part2)
