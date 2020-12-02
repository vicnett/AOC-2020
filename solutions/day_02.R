# Advent of code 2020
# Day 2

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

inputData <- read_lines("../inputs/day_02")

# Part 1 problem:
# How many passwords in the list are valid based on the associated password policy?
# Data structure:
# x-y z: abcde
# Where
#   x is the minimum required instances of the given character
#   y is the maximum allowed instances of the given characters
#   z is the required character
#   abcde is the password itself

# Input data preprocessing

inputAsTibble <- as_tibble(str_split_fixed(inputData, " ", 3))

colnames(inputAsTibble) <- c(
    "range",
    "character",
    "password"
  )

processedInput <- inputAsTibble %>%
  separate(range, into = c(
    "min",
    "max"
  )) %>%
  mutate(
    min = as.numeric(min),
    max = as.numeric(max)
  ) %>%
  mutate(character = str_replace(character, ":", ""))

# Check if there are enough and/or too many of the required characters

passwordCheck <- processedInput %>%
  mutate(
    enough = str_detect(password, paste0("(", character, ".*){", min, ",", max, "}")),
    too_many = str_detect(password, paste0("(", character, ".*){", max + 1, ",}"))
  ) %>%
  filter(
    enough,
    !too_many
  )

# Count how many are valid

print(paste0("There are ", length(passwordCheck$password), " valid passwords in the list."))

# Part 2 problem:
# How many passwords in the list are valid based on the associated password policy?
# Data structure:
# x-y z: abcde
# Where
#   x is the first position where the required character is expected
#   y is the second position where the required character is expected
#   z is the required character
#   abcde is the password itself

# Input data preprocessing

inputAsTibble <- as_tibble(str_split_fixed(inputData, " ", 3))

colnames(inputAsTibble) <- c(
  "range",
  "character",
  "password"
)

processedInput <- inputAsTibble %>%
  separate(range, into = c(
    "first",
    "second"
  )) %>%
  mutate(
    first = as.numeric(first),
    second = as.numeric(second)
  ) %>%
  mutate(character = str_replace(character, ":", ""))

# Check if there are enough and/or too many of the required characters

passwordCheck <- processedInput %>%
  mutate(
    first_ok = str_sub(password, start = first, end = first) == character,
    second_ok = str_sub(password, start = second, end = second) == character
  ) %>%
  filter(
    first_ok + second_ok == 1
  )

# Count how many are valid

print(paste0("There are ", length(passwordCheck$password), " valid passwords in the list."))
