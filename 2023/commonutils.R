is.digit <- function(chr) {
  return(nchar(chr) == 1 && grepl("[0-9]", chr))
}

custom_sort <- function(l, cmp) {
  tmp <- l
  for (i in 1:(length(l) - 1)) {
    min <- find_min_index(tmp, cmp)
    l <- swap(l, i, min + i - 1)
    tmp <- l[-c(1:i)]
  }
  return(l)
}

find_min_index <- function(l, cmp) {
  min <- l[[1]]
  min_index <- 1
  for (i in 2:length(l)) {
    if (cmp(l[[i]], min) < 0) {
      min <- l[[i]]
      min_index <- i
    }
  }
  return(min_index)
}

swap <- function(l, i1, i2) {
  tmp <- l[[i1]]
  l[[i1]] <- l[[i2]]
  l[[i2]] <- tmp
  return(l)
}

comp <- function(a, b) { return(a - b) }

l <- c(4, 5, 1, 3, 9, 2, 6, 8, 7)

custom_sort(l, comp)