# Advent of code 2020
# Day 3

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

inputData <- read_lines("../inputs/day_03")

# Part 1:
# The input is a map of a slope with
#   - empty spots, represented by a period .
#   - trees, represented by an octothorpe #
# The slope terrain extends to the right by repeating the same map.
# Count how many trees are encountered when starting from the top-left-most empty spot, and descending on a "3 right, one down" slope.

findTreesOnSlope <- function(
  slope_map,   # problem input
  slope_right, # how many spots to the right the slope goes on each step
  slope_down   # how many spots down the slope goes on each step
) {
  
  map_width <- nchar(slope_map[[1]])
  
  trees_on_each_step <- as_tibble(inputData) %>%
    # keep only rows we land on
    filter((row_number() - 1) %% slope_down == 0) %>%
    # Check if we see a tree at each step
    mutate(
      absolute_position = (slope_right * (row_number() - 1) + 1),
      overflow_count = floor((absolute_position - 1) / map_width),
      relative_position = absolute_position - (map_width * overflow_count),
      tree_found = str_sub(
        value,
        start = relative_position,
        end = relative_position
      ) == "#"
    )
  
}

partOneTrees <- findTreesOnSlope(inputData, 3, 1)

print(paste0("A toboggan going 3 over and 1 down encounters ", sum(partOneTrees$tree_found), " trees on its way down the slope"))

# Part 2:
# Count how many trees are encountered when starting from the top-left-most empty spot, and descending on the following slope patterns:
#   - Right 1, down 1.
#   - Right 3, down 1. (This is the slope from step 1)
#   - Right 5, down 1.
#   - Right 7, down 1.
#   - Right 1, down 2.
# Then multiply the answers together

partTwoTrees <- 
  sum(findTreesOnSlope(inputData, 1, 1)$tree_found) *
  sum(findTreesOnSlope(inputData, 3, 1)$tree_found) *
  sum(findTreesOnSlope(inputData, 5, 1)$tree_found) *
  sum(findTreesOnSlope(inputData, 7, 1)$tree_found) *
  sum(findTreesOnSlope(inputData, 1, 2)$tree_found)

print(paste0("I don't feel like coming up with another pretty string here. Just take your number and go away: ", partTwoTrees))
