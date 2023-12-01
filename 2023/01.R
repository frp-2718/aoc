library(stringr)

f <- file("01.txt", "r")
input <- readLines(f)
close(f)

spelled_out_digits <- c("one" = "o1e", "two" = "t2o", "three" = "t3e", "four" = "f4r",
                        "five" = "f5e", "six" = "s6x", "seven" = "s7n", "eight" = "e8t",
                        "nine" = "n9e")

extract_value = function(str) {
  s <- strtoi(strsplit(str, NULL)[[1]])
  ints <- s[!is.na(s)]
  return(ints[[1]] * 10 + tail(ints, 1))
}

part1 <- Reduce("+", lapply(input, extract_value), 0)
part2 <- Reduce("+", lapply(str_replace_all(input, spelled_out_digits), extract_value), 0)
