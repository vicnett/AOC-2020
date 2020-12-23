# Advent of code 2020
# Day 12

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

# The input consists of a sequence of single-character actions
# paired with integer input values:
  
#   - N means move north by the given value.
#   - S means move south by the given value.
#   - E means move east by the given value.
#   - W means move west by the given value.
#   - L means turn left the given number of degrees.
#   - R means turn right the given number of degrees.
#   - F means move forward by the given value in the direction the ship is currently facing.

# The ship starts out facing east. Only the L and R actions change the direction the ship is facing.

input_data <- read_lines("../inputs/day_12")
sample_input_data <- read_lines("F10
N3
F7
R90
F11")

# Part 1

# What is the Manhattan distance between the ship's starting position
# and its position after following the input instructions?

# First, let's get the input in a more usable state
nav_instructions <- tibble(
  action = str_sub(input_data, start = 1, end = 1),
  value = as.numeric(str_sub(input_data, start = 2, end = -1))
)

# Then let's write a helper function for turning the ship

turn_ship <- function(from, dir, degrees) {
  
  directions <- list(
    "N",
    "E",
    "S",
    "W"
  )
  
  dirs <- list(
    "L",
    "R"
  )
  
  stopifnot(from %in% directions)
  stopifnot(dir %in% dirs)
  stopifnot(degrees %% 90 == 0)
  
  num_turns <- degrees / 90
  
  current_direction <- which(directions == from)
  
  if (num_turns %% 4 == 0) {
    return(from)
  } else if (dir == "R") {
    return(rep_len(directions, current_direction + 4 + num_turns)[[current_direction + 4 + num_turns]])
  } else if (dir == "L") {
    return(rep_len(directions, current_direction + 4 - num_turns)[[current_direction + 4 - num_turns]])
  }
  
}

calculate_next_coords <- function(action, value, direction, coords) {
  
  east_west_position <- coords[[1]]
  north_south_position <- coords[[2]]
  
  next_east_west_position =
    if_else(
      action == "E",
      east_west_position + value,
      if_else(
        action == "W",
        east_west_position - value,
        if_else(
          action == "F" & direction == "E",
          east_west_position + value,
          if_else(
            action == "F" & direction == "W",
            east_west_position - value,
            east_west_position
          )
        )
      )
    )
  
  next_north_south_position =
    if_else(
      action == "S",
      north_south_position - value,
      if_else(
        action == "N",
        north_south_position + value,
        if_else(
          action == "F" & direction == "S",
          north_south_position - value,
          if_else(
            action == "F" & direction == "N",
            north_south_position + value,
            north_south_position
          )
        )
      )
    )
  
  return(list(next_east_west_position, next_north_south_position))
  
}

ship_path <- nav_instructions %>%
  # Initialize starting parameters
  mutate(
    direction = "E",
    east_west_position = 0,
    north_south_position = 0
  )

# Giving in and using a loop again -_-

for (n in 1:nrow(ship_path)) {
  this_action <- ship_path$action[[n]]
  this_value <- ship_path$value[[n]]
  
  last_direction <- lag(ship_path$direction, default = "E")[[n]]
  last_east_west_position <- lag(ship_path$east_west_position, default = 0)[[n]]
  last_north_south_position <- lag(ship_path$north_south_position, default = 0)[[n]]
  
  if (this_action %in% c("L", "R")) {
    ship_path$direction[[n]] <- turn_ship(
      from = last_direction,
      dir = this_action,
      degrees = this_value
    )
    ship_path$east_west_position[[n]] <- last_east_west_position
    ship_path$north_south_position[[n]] <- last_north_south_position
  } else {
    next_positions <- calculate_next_coords(this_action, this_value, last_direction, c(last_east_west_position, last_north_south_position))
    ship_path$east_west_position[[n]] <- next_positions[[1]]
    ship_path$north_south_position[[n]] <- next_positions[[2]]
    ship_path$direction[[n]] <- last_direction
  }
}

final_east_west_position <- ship_path$east_west_position[[nrow(ship_path)]]
final_north_south_position <- ship_path$north_south_position[[nrow(ship_path)]]

part_one_answer <- abs(final_east_west_position) + abs(final_north_south_position)

print(paste0("After following the navigation instructions, the ship's Manhattan distance from origin is ", part_one_answer, "."))

# Part 2

# Oh whoops no that's not at all what the instructions mean!!! ¬_¬
# They don't tell you how to move the ship, they tell you how to move an imaginary (lol)
# waypoint relative to the ship's position...

#   - N means move the waypoint north by the given value.
#   - S means move the waypoint south by the given value.
#   - E means move the waypoint east by the given value.
#   - W means move the waypoint west by the given value.
#   - L means rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
#   - R means rotate the waypoint around the ship right (clockwise) the given number of degrees.
#   - F means move the ship to the waypoint by the given value.

# The waypoint starts 10 units east and 1 unit north relative to the ship.
# The waypoint is relative to the ship; that is, if the ship moves, the waypoint moves with it.

# After following the instructions using this new interpretation, what will the ship's Manhattan distance
# from its starting point be?

# The nav_instructions are fine as-is but I need to tweak all my other code :( Hopefully not by much!

rotate_waypoint <- function(waypoint, dir, degrees) {
  # This one should be simpler. Once we know the net effect of the instruction we should be able to
  # just change the signs of the waypoint's coordinates...
  
  dirs <- list(
    "L",
    "R"
  )
  
  stopifnot(dir %in% dirs)
  stopifnot(degrees %% 90 == 0)
  
  num_steps <- degrees / 90
  
  rotation <- if (dir == "L") -num_steps %% 4 else if (dir == "R") num_steps %% 4
  
  if (rotation == 0) {
    return(waypoint)
  } else if (rotation %% 2 == 0) {
    # This would be a 180 so both coordinates get inverted
    return(-waypoint)
  } else if (abs(rotation) == 1) {
    # This is either 90 degrees clockwise or counter-clockwise
    # Clockwise: x gets inverted
    if (rotation > 0) return(c(waypoint[[2]], -waypoint[[1]]))
    # Counterclockwise: y gets inverted
    if (rotation < 0) return(c(-waypoint[[2]], waypoint[[1]]))
  }
  else if (abs(rotation) == 3) {
    # This is either 90 degrees clockwise or counter-clockwise
    # But the opposite from above
    # Clockwise: x gets inverted
    if (rotation > 0) return(c(-waypoint[[2]], waypoint[[1]]))
    # Counterclockwise: y gets inverted
    if (rotation < 0) return(c(waypoint[[2]], -waypoint[[1]]))
  }
  
}

move_waypoint <- function(action, value, waypoint) {
  
  waypoint_x <- waypoint[[1]]
  waypoint_y <- waypoint[[2]]
  
  next_east_west_position =
    if_else(
      action == "E",
      waypoint_x + value,
      if_else(
        action == "W",
        waypoint_x - value,
        waypoint_x
      )
    )
  
  next_north_south_position =
    if_else(
      action == "S",
      waypoint_y - value,
      if_else(
        action == "N",
        waypoint_y + value,
        waypoint_y
      )
    )
  
  return(list(next_east_west_position, next_north_south_position))
  
}

ship_path <- nav_instructions %>%
  # Initialize starting parameters
  mutate(
    waypoint_x = 10,
    waypoint_y = 1,
    east_west_position = 0,
    north_south_position = 0
  )

# Here we go again!!

for (n in 1:nrow(ship_path)) {
  this_action <- ship_path$action[[n]]
  this_value <- ship_path$value[[n]]
  
  last_waypoint_x <- lag(ship_path$waypoint_x, default = 10)[[n]]
  last_waypoint_y <- lag(ship_path$waypoint_y, default = 1)[[n]]
  last_waypoint <- c(last_waypoint_x, last_waypoint_y)
  
  last_east_west_position <- lag(ship_path$east_west_position, default = 0)[[n]]
  last_north_south_position <- lag(ship_path$north_south_position, default = 0)[[n]]
  
  if (this_action %in% c("L", "R")) {
    new_waypoint <- rotate_waypoint(
      waypoint = last_waypoint,
      dir = this_action,
      degrees = this_value
    )
    ship_path$waypoint_x[[n]] <- new_waypoint[[1]]
    ship_path$waypoint_y[[n]] <- new_waypoint[[2]]
    ship_path$east_west_position[[n]] <- last_east_west_position
    ship_path$north_south_position[[n]] <- last_north_south_position
  } else if (this_action %in% c("N", "S", "E", "W")) {
    next_waypoint <- move_waypoint(this_action, this_value, last_waypoint)
    ship_path$waypoint_x[[n]] <- next_waypoint[[1]]
    ship_path$waypoint_y[[n]] <- next_waypoint[[2]]
    ship_path$east_west_position[[n]] <- last_east_west_position
    ship_path$north_south_position[[n]] <- last_north_south_position
  } else if (this_action == "F") {
    ship_path$waypoint_x[[n]] <- last_waypoint[[1]]
    ship_path$waypoint_y[[n]] <- last_waypoint[[2]]
    ship_path$east_west_position[[n]] <- last_east_west_position + (last_waypoint_x * this_value)
    ship_path$north_south_position[[n]] <- last_north_south_position + (last_waypoint_y * this_value)
  }
}

final_east_west_position <- ship_path$east_west_position[[nrow(ship_path)]]
final_north_south_position <- ship_path$north_south_position[[nrow(ship_path)]]

part_two_answer <- abs(final_east_west_position) + abs(final_north_south_position)

print(paste0("After following the REVISED navigation instructions, the ship's Manhattan distance from origin is ", part_two_answer, "."))