# Advent of code 2020
# Day 1

# Load libraries

library(plyr)
library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

inputData <- as.numeric(read_lines("../inputs/day_01"))

# Part 1:
# Find the two entries that sum to 2020 and multiply them together

# Find all possible combinations of up to two numbers from the list
combos <- lapply(1:2, function(x) combn(inputData, x))

# Arrange combos into two columns
sums <- as_tibble(rbind.fill.matrix(sapply(combos, function(x) t(x)))) %>%
  # Add columns with the sums and products of each combo
  mutate(
    sum = `1` + `2`,
    product = `1` * `2`
    ) %>%
  # find the one we want
  filter(sum == 2020)

# Answer

print(paste0("Part 1 answer is: ", sums$product))


# Part 2:
# Find the three entries that sum to 2020 and multiply them together

# Find all possible combinations of up to three numbers from the list
combos <- lapply(1:3, function(x) combn(inputData, x))

# Arrange combos into two columns
sums <- as_tibble(rbind.fill.matrix(sapply(combos, function(x) t(x)))) %>%
  # Add columns with the sums and products of each combo
  mutate(
    sum = `1` + `2` + `3`,
    product = `1` * `2` * `3`
  ) %>%
  # find the one we want
  filter(sum == 2020)

# Answer

print(paste0("Part 2 answer is: ", sums$product))

# Refactor

productOfValuesAddingUpToTarget <- function(
  input, # vector of integers
  sum_count, # how many need to be summed
  target
) {
  
  # Find all possible combinations of up to two numbers from the list
  combos <- lapply(1:sum_count, function(x) combn(input, x))
  
  # Arrange combos into two columns
  sums <- as_tibble(rbind.fill.matrix(sapply(combos, function(x) t(x)))) %>%
    # Remove combos that don't have as many numbers as we want
    drop_na() %>%
    # Add columns with the sums and products of each combo
    rowwise() %>%
    mutate(
      sum = sum(c_across(1:sum_count)),
      product = prod(c_across(1:sum_count))
    ) %>%
    # find the one we want
    filter(sum == target)
  
  # Answer
  
  return(sums$product[[1]])
  
}

print(paste0("Part 1 answer is: ", productOfValuesAddingUpToTarget(inputData, 2, 2020)))
print(paste0("Part 2 answer is: ", productOfValuesAddingUpToTarget(inputData, 3, 2020)))