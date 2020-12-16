# Advent of code 2020
# Day 11

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

# Input is a representation of a ferry boarding waiting area with seats arranged on an even grid
# Each character is either
#   . the floor
#   L an empty seat
#   # an occupied seat

# All seats start out empty (so, no octothorpes in the problem input!)

# Waiting passengers act according to a very simple model where during step/turn/tick, the following happen:
#   - Empty seats (L) with no occupied seats adjacent to them become occupied (#)
#   - Occupied seats (#) with four or more seats adjacent to them also occupied become empty (L)
#   - Otherwise, the seat's state does not change
# Note: "adjacent" here means all cardinal directions plus diagonals.

# It is expected that this "model" will eventually stabilize and no further changes will happen after a certain number of steps.

input_data <- read_lines("../inputs/day_11")
sample_input_data <- read_lines("L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

# Part 1

# How many seats end up being occupied once no further changes occur?

# Let's convert the seat layout into a matrix
seat_map <- str_replace_all(input_data, c("L" = "1", "\\." = "0")) %>%
  sapply(str_split, "", simplify = TRUE) %>%
  sapply(as.numeric) %>%
  unname()

# Let's also initialize a map of occupied seats
occupied_seat_map <- matrix(data = 0, nrow = nrow(seat_map), ncol = ncol(seat_map))

# Write a function to count how many neighbors each spot on the map has
count_neighbors <- function(seat_map) {
  
  shift_north <- function(x) {
    rbind(x[2:nrow(x), ], rep(0, ncol(x)))
  }
  shift_south <- function(x) {
    rbind(rep(0, ncol(x)), x[1:(nrow(x) - 1), ])
  }
  shift_east <- function(x) {
    cbind(rep(0, nrow(x)), x[, 1:(ncol(x) - 1)])
  }
  shift_west <- function(x) {
    cbind(x[, 2:ncol(x)], rep(0, nrow(x)))
  }
  
  shifted_north <- shift_north(seat_map)
  shifted_south <- shift_south(seat_map)
  shifted_east <- shift_east(seat_map)
  shifted_west <- shift_west(seat_map)
  shifted_northeast <- shift_east(shifted_north)
  shifted_northwest <- shift_west(shifted_north)
  shifted_southeast <- shift_east(shifted_south)
  shifted_southwest <- shift_west(shifted_south)
  
  return(
    shifted_north +
    shifted_south +
    shifted_east +
    shifted_west +
    shifted_northeast +
    shifted_northwest +
    shifted_southeast +
    shifted_southwest
  )
  
}

# And a function to apply the "model" until there is no change
run_seating_simulation <- function(seat_map, occupied_seat_map) {
  
  # Count how many neighboring seats are occupied
  occupied_neighbors <- count_neighbors(occupied_seat_map)
  # Apply the rules of the simulation to figure out the next step
  next_occupied_seat_map <- occupied_seat_map
  next_occupied_seat_map[next_occupied_seat_map == 0 & occupied_neighbors == 0 & seat_map == 1] <- 1
  next_occupied_seat_map[next_occupied_seat_map == 1 & occupied_neighbors >= 4 & seat_map == 1] <- 0
  
  # If there's no change, we're done
  if (all(next_occupied_seat_map == occupied_seat_map)) {
    return(occupied_seat_map)
  } else {
    # Otherwise, calculate one more step
    run_seating_simulation(seat_map, next_occupied_seat_map)
  }
  
}

post_sim_map <- run_seating_simulation(seat_map, occupied_seat_map)

print(paste0("After running the simulation until there are no further changes, ", sum(post_sim_map), " seats are occupied."))

# Part 2

# Different model:
# Passengers now consider the first seat they can see from a given position, extending out in all eight directions
# They also will only vacate their seat if five or more of these seats are taken.
# All other rules remain unchanged.

# Once the simulation stabilizes with these new rules, how many seats are occupied?

# I guess I get to re-write my functions :/

count_visible_neighbors <- function(seat_map, occupied_seat_map, x, y) {
  
  check_seat <- function(x, y) {
    if (seat_map[[y, x]] == 1) {
      return(occupied_seat_map[[y, x]] + 1)
    } else return(0)
  }
  
  look_north = if (y == 1) list(0) else  lapply(1:(y - 1), function(n) check_seat(x, y - n))
  look_south = if (y == nrow(seat_map)) list(0) else  lapply(1:(nrow(seat_map) - y), function(n) check_seat(x, y + n))
  look_east = if (x == ncol(seat_map)) list(0) else  lapply(1:(ncol(seat_map) - x), function(n) check_seat(x + n, y))
  look_west = if (x == 1) list(0) else  lapply(1:(x - 1), function(n) check_seat(x - n, y))
  look_northeast = if (y == 1 | x == ncol(seat_map)) list(0) else  lapply(1:min(y - 1, ncol(seat_map) - x), function(n) check_seat(x + n, y - n))
  look_northwest = if (y == 1 | x == 1) list(0) else  lapply(1:min(y - 1, x - 1), function(n) check_seat(x - n, y - n))
  look_southeast = if (y == nrow(seat_map) | x == ncol(seat_map)) list(0) else  lapply(1:min(nrow(seat_map) - y, ncol(seat_map) - x), function(n) check_seat(x + n, y + n))
  look_southwest = if (y == nrow(seat_map) | x == 1) list(0) else  lapply(1:min(nrow(seat_map) - y, x - 1), function(n) check_seat(x - n, y + n))
  
  visible_seat_count <- sum(
    look_north[[min(which(look_north > 0))]] == 2,
    look_south[[min(which(look_south > 0))]] == 2,
    look_east[[min(which(look_east > 0))]] == 2,
    look_west[[min(which(look_west > 0))]] == 2,
    look_northeast[[min(which(look_northeast > 0))]] == 2,
    look_northwest[[min(which(look_northwest > 0))]] == 2,
    look_southeast[[min(which(look_southeast > 0))]] == 2,
    look_southwest[[min(which(look_southwest > 0))]] == 2
  )
  
  return(visible_seat_count)
  
}

foo <- count_visible_neighbors(seat_map, occupied_seat_map, 2, 10)

run_line_of_sight_seating_simulation <- function(seat_map, occupied_seat_map) {
  
  # Count how many occupied seats are visible from each position
  visible_neighbors <- matrix(data = 0, nrow = nrow(seat_map), ncol = ncol(seat_map))
  for (x in 1:ncol(visible_neighbors)) {
    for (y in 1:nrow(visible_neighbors)) {
      visible_neighbors[[y, x]] <- count_visible_neighbors(seat_map, occupied_seat_map, x, y)
    }
  }
  # Apply the rules of the simulation to figure out the next step
  next_occupied_seat_map <- occupied_seat_map
  next_occupied_seat_map[next_occupied_seat_map == 0 & visible_neighbors == 0 & seat_map == 1] <- 1
  next_occupied_seat_map[next_occupied_seat_map == 1 & visible_neighbors >= 5 & seat_map == 1] <- 0
  
  # If there's no change, we're done
  if (all(next_occupied_seat_map == occupied_seat_map)) {
    return(occupied_seat_map)
  } else {
    # Otherwise, calculate one more step
    run_line_of_sight_seating_simulation(seat_map, next_occupied_seat_map)
  }
  
}

post_line_of_sight_sim_map <- run_line_of_sight_seating_simulation(seat_map, occupied_seat_map)

print(paste0("After running the second simulation until there are no further changes, ", sum(post_line_of_sight_sim_map), " seats are occupied."))
