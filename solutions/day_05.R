# Advent of code 2020
# Day 5

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

inputData <- read_lines("../inputs/day_05")

# Part 1
# The input is a list of "boarding passes" which use binary space partitioning to encode seat numbers.
# Each pass is 10 characters long. The first 7 will be a combination of F and B and represent one of the 128 rows on the plane.
# Each letter indicates whether to take the (F)ront or (B)ack half of remaining rows.
# The last three characters will be a combination of L and R to indicate one of 8 columns.
# Similarly each letter indicates whether to take the (L)eft or (R)ight half of remaining columns.
# Rows and columns are zero-indexed. Dammit...
# Each seat also has a seat ID which is calculated by multiplying the row by 8 and adding the column.
# The output of part 1 should be the highest seat ID on a boarding pass.

# So, let's get to decoding these thangs!

boardingPasses <- as_tibble(inputData) %>%
  # Because I feel like it
  rename(raw_pass = value) %>%
  # Separate the string into row and column codes, and convert to binary strings
  mutate(
    row = str_replace_all(str_sub(raw_pass, start = 1, end = 7), c("F" = "0", "B" = "1")),
    col = str_replace_all(str_sub(raw_pass, start = 8, end = -1), c("L" = "0", "R" = "1"))
  ) %>%
  # Then convert to base10
  mutate(
    row_num = strtoi(row, base = 2),
    col_num = strtoi(col, base = 2)
  ) %>%
  # Then calculate the seat ID
  mutate(
    seat_id = row_num * 8 + col_num
  )

print(paste0("The highest seat ID is: ", max(boardingPasses$seat_id), "."))

# Part 2
# Find your own seat ID. There's some non-existent seats in the front and back, but that's not where you're sitting.
# So your seat ID is going to be the one missing one somewhere in the list.

orderedBoardingPasses <- boardingPasses %>%
  # order passes by seat ID
  arrange(seat_id) %>%
  # calculate offset between row number and seat ID
  mutate(offset = seat_id - row_number()) %>%
  # put seats with highest offset first, still ordered by seat ID
  arrange(desc(offset), seat_id)

# Boom, missing seat is one less than the top-sorted one
print(paste0("Your seat ID is ", orderedBoardingPasses$seat_id[[1]] - 1, "."))