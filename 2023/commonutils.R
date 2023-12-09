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

gcd <- function(a, b) {
  a <- abs(a)
  b <- abs(b)
  if (a < b) {
    tmp <- a
    a <- b
    b <- tmp
  }
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}

gcd_v <- function(v) {
  if (length(v) == 0) {
    return(0)
  } 
  res <- 0
  for (n in v) {
    res <- gcd(res, n)
  }
  return(res)
}

lcm <- function(a, b) {
  if (a == 0 && b == 0) return (0)
  return(abs(a) * (abs(b) / gcd(a, b)))
}

lcm_v <- function(v) {
  if (length(v) == 0) {
    return(0)
  } 
  res <- 1
  for (n in v) {
    res <- lcm(res, n)
  }
  return(res)
}