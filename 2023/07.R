source("commonutils.R")

library(purrr)
library(readr)
library(stringr)

input <- readLines("test-inputs/07-test.txt")

cards <- c("A" = "14", "K" = "13", "Q" = "12", "J" = "11", "T" = "10")
cards_with_jokers <- c("A" = "14", "K" = "13", "Q" = "12", "J" = "1", "T" = "10")

hands <- function(mapping) {
  res <- strsplit(input, " +") |>
    map(function(x) c(strsplit(x[[1]], ""), x[[2]])) |>
    map(function(x) c(str_replace_all(c(x[[1]]), mapping), x[[2]])) |>
    map(parse_integer)
  return(res)
}

get_strength <- function(hand) {
  if (hand[[6]] < 15) {
    hand[[6]] <- 100
  }
  hand <- sort(hand)
  last <- hand[[1]]
  counter <- 1
  i <- 2
  cards <- c(0, 0, 0, 0, 0)
  for (card in hand[-c(1)]) {
    if (card == last) {
      counter <- counter + 1
    } else {
      cards[[counter]] <- cards[[counter]] + 1
      counter <- 1
    }
    last <- card
    i <- i + 1
  }
  if (cards[[5]] == 1) {
    return(7)
  } else if (cards[[1]] == 1 && cards[[4]] == 1) {
    return(6)
  } else if (cards[[2]] == 1 && cards[[3]] == 1) {
    return(5)
  } else if (cards[[1]] == 2 && cards[[3]] == 1) {
    return(4)
  } else if (cards[[1]] == 1 && cards[[2]] == 2) {
    return(3)
  } else if (cards[[1]] == 3) {
    return(2)
  } else {
    return(1)
  }
}

cmp <- function(h1, h2) {
  score1 <- h1[[7]]
  score2 <- h2[[7]]
  if (score1 != score2) {
    return(score1 - score2)
  } else {
    for (i in 1:length(h1)) {
      if (h1[[i]] != h2[[i]]) {
        return(h1[[i]] - h2[[i]])
      }
    }
  }
  return(0)
}

get_total_winnings <- function(hands) {
  total <- 0L
  for (i in 1:length(hands)) {
    total <- total + (i * hands[[i]][[6]])
  }
  return(total)
}

get_strength_with_jokers <- function(hand) {
  hand <- hand[-c(6, 7)]
  cards <- rev(sort(table(hand)))
  njacks <- try(cards[['1']], silent = TRUE)
  if (class(njacks) == "try-error") {
    njacks <- 0
  } else {
    njacks <- cards[['1']]
  }
  cards[['1']] <- 0
  cards <- rev(sort(cards))

  if (njacks == 5) {
    return(7)
  } else {
    cards[[1]] <- cards[[1]] + njacks
  }

  if (cards[[1]] == 5) {
    return(7)
  } else if (cards[[1]] == 4) {
    return(6)
  } else if (cards[[1]] == 3 && cards[[2]] == 2) {
    return(5)
  } else if (cards[[1]] == 3) {
    return(4)
  } else if (cards[[1]] == 2 && cards[[2]] == 2) {
    return(3)
  } else if (cards[[1]] == 2) {
    return(2)
  } else {
    return(1)
  }
}

valued_hands <- map(hands(cards), function(x) append(x, get_strength(x)))
valued_hands_with_jokers <- map(hands(cards_with_jokers), function(x) append(x, get_strength_with_jokers(x)))

part1 <- get_total_winnings(custom_sort(valued_hands, cmp))
part2 <- get_total_winnings(custom_sort(valued_hands_with_jokers, cmp))

print(part1)
print(part2)
