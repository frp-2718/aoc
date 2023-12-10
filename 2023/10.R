input <- readLines("test-inputs/10-test-4.txt")
grid <- do.call(rbind, strsplit(input, ""))

curve <- grid

walk <- function() {
  start <- which(grid == "S", arr.ind = TRUE)
  neighbors <- get_neighbors(start)
  steps <- 0
  
  if (neighbors[['up']] == "|" || neighbors[['up']] == "7" || neighbors[['up']] == "F") {
    first_step <- c(row = start[[1]] - 1, col = start[[2]])
  } else if (neighbors[['right']] == "-" || neighbors[['right']] == "7" || neighbors[['right']] == "J") {
    first_step <- c(row = start[[1]], col = start[[2]] + 1)
  } else if (neighbors[['down']] == "|" || neighbors[['down']] == "L" || neighbors[['down']] == "J") {
    first_step <- c(row = start[[1]] + 1, col = start[[2]])
  } else {
    first_step <- c(row = start[[1]], col = start[[2]] - 1)
  }
  
  prev_pos <- c(row = start[[1]], col = start[[2]])
  current_step <- first_step
  current_pipe <- ""
  curve[prev_pos[["row"]], prev_pos[["col"]]] <<- "X"

  while (current_pipe != "S") {
    next_step <- get_next_step(prev_pos, current_step)
    prev_pos <- current_step
    current_step <- next_step
    steps <- steps + 1
    current_pipe <- grid[prev_pos[['row']], prev_pos[['col']]]
    curve[prev_pos[["row"]], prev_pos[["col"]]] <<- "X"
  }
  
  return(steps)
}

get_neighbors <- function(pos) {
  up <- ifelse(pos[[1]] == 1, "", grid[pos[[1]] - 1, pos[[2]]])
  right <- ifelse(pos[[2]] == length(grid[1, ]), "", grid[pos[[1]], pos[[2]] + 1])
  down <- ifelse(pos[[1]] == length(grid[, 1]), "", grid[pos[[1]] + 1, pos[[2]]])
  left <- ifelse(pos[[2]] ==1, "", grid[pos[[1]], pos[[2]] - 1])
  return(c(up = up, right = right, down = down, left = left))
}

get_next_step <- function(pos, pipe_pos) {
  pipe <- grid[[pipe_pos[['row']], pipe_pos[['col']]]]
  if (pipe == "-") {
    if (pipe_pos[['col']] > pos[['col']])
      return(c(row = pipe_pos[['row']], col = pipe_pos[['col']] + 1))
    else
      return(c(row = pipe_pos[['row']], col = pipe_pos[['col']] - 1))
  } else if (pipe == "7") {
    if (pipe_pos[['col']] > pos[['col']])
      return(c(row = pipe_pos[['row']] + 1, col = pipe_pos[['col']]))
    else
      return(c(row = pipe_pos[['row']], col = pipe_pos[['col']] - 1))
  } else if (pipe == "|") {
    if (pipe_pos[['row']] > pos[['row']])
      return(c(row = pipe_pos[['row']] + 1, col = pipe_pos[['col']]))
    else
      return(c(row = pipe_pos[['row']] - 1, col = pipe_pos[['col']]))
  } else if (pipe == "F") {
    if (pipe_pos[['row']] < pos[['row']])
      return(c(row = pipe_pos[['row']], col = pipe_pos[['col']] + 1))
    else
      return(c(row = pipe_pos[['row']] + 1, col = pipe_pos[['col']]))
  } else if (pipe == "J") {
    if (pipe_pos[['col']] > pos[['col']])
      return(c(row = pipe_pos[['row']] - 1, col = pipe_pos[['col']]))
    else
      return(c(row = pipe_pos[['row']], col = pipe_pos[['col']] - 1))
  } else {
    if (pipe_pos[['row']] > pos[['row']])
      return(c(row = pipe_pos[['row']], col = pipe_pos[['col']] + 1))
    else
      return(c(row = pipe_pos[['row']] - 1, col = pipe_pos[['col']]))
  }
}

enclosed <- function(curve) {
  inside <- FALSE
  total <- 0
  rows <- dim(curve)[[1]]
  cols <- dim(curve)[[2]]
  for (row in 1:rows) {
    for (col in 1:cols) {
      if (curve[row, col] == "X") {
        pipe <- grid[row, col]
        if (pipe == "S") {
          inside <- TRUE
        } else if (pipe == "|" || pipe == "J" || pipe == "L") {
          if (inside) inside <- FALSE
          else inside <- TRUE
        }
      } else {
        if (inside) {
          total <- total + 1
        }
      }
    }
    inside <- FALSE
  }
  return(total)
}

part1 <- walk() / 2
part2 <- enclosed(curve)

print(part1)
print(part2)
