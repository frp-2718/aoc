library(stringr)

input1 <- readLines("test-inputs/01-test.txt")
input2 <- readLines("test-inputs/01-test-2.txt")

spelled_out_digits <- c("one" = "o1e", "two" = "t2o", "three" = "t3e", "four" = "f4r",
                        "five" = "f5e", "six" = "s6x", "seven" = "s7n", "eight" = "e8t",
                        "nine" = "n9e")

extract_value = function(str) {
  s <- strtoi(strsplit(str, NULL)[[1]])
  ints <- s[!is.na(s)]
  return(ints[[1]] * 10 + tail(ints, 1))
}

part1 <- Reduce("+", lapply(input1, extract_value), 0)
part2 <- Reduce("+", lapply(str_replace_all(input2, spelled_out_digits), extract_value), 0)

print(part1)
print(part2)
