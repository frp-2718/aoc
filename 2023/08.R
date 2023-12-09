source("commonutils.R")

input_file <- "test-inputs/08-test.txt"

input <- read.delim(input_file, skip = 1, header = FALSE, sep = " ")
colnames(input) <- c("location", "equal", "L", "R")

network <- subset(input, select = -equal) |>
  lapply(function(x) gsub("[^A-Z0-9]", "", x)) |>
  as.data.frame()

rownames(network) <- network$location
network <- subset(network, select = -location)

instructions <- unlist(strsplit(readLines(input_file, n = 1), ""))

cycles <- function(dests) {
  step <- 1L
  ndests <- length(dests)
  cycle <- c()
  while (ndests > 0) {
    i <- step
    i <- ((i - 1) %% length(instructions)) + 1
    instr <- instructions[[i]]
    
    for (d in 1:length(dests)) {
      new_dest <- network[dests[[d]], ][[instr]]
      dests[[d]] <- new_dest
      if (substring(new_dest, 3, 3) == "Z") {
        cycle <- append(cycle, step)
        ndests <- ndests - 1
      }
    }
    step <- step + 1
  }
  return(cycle)
}

dests_1 <- c("AAA")

dests_2 <- c()
for (name in row.names(network)) {
  if (substring(name, 3, 3) == "A") {
    dests_2 <- append(dests, name)
  }
}

part1 <- lcm_v(cycles(dests_1))
part2 <- lcm_v(cycles(dests_2))

print(format(part1, scientific = FALSE))
print(format(part2, scientific = FALSE))
