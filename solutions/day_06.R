# Advent of code 2020
# Day 6
# Tried to go fast so this one is super messy
# Note: comments were added after completing, but I'm not cleaning up the code

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input
# Each line represents which questions a single person answered "yes" to (each question is represented by a single letter)
# Groups of people are separated by a blank line.

inputData <- read_file("../inputs/day_06")

# Part 1
# For each group, count how many questions were answered yes by anyone.
# What is the sum of these counts?

# I totally reused some of the passport processing code from day 4

answerGroupList <- str_replace_all(str_split(inputData, '\\n\\n')[[1]], "\\n", " ") %>%
  as_tibble() %>%
  mutate(
    group_id = row_number(),
    value = str_split(value, " ")
  ) %>%
  unnest(value) %>%
  group_by(group_id) %>%
  summarise(yes_answers = paste0(value, collapse = "")) %>%
  separate_rows(yes_answers, sep = "") %>%
  filter(yes_answers != "") %>%
  group_by(group_id, yes_answers) %>%
  filter(row_number() == 1) %>%
  group_by(group_id) %>%
  summarise(unique_yes_answers = n())

# Part 2
# Whoops, we don't want questions anyone answered yes to, we want questions everyone answered yes to!

answerGroupList <- str_replace_all(str_split(inputData, '\\n\\n')[[1]], "\\n", " ") %>%
  as_tibble() %>%
  mutate(
    group_id = row_number(),
    value = str_split(value, " ")
  ) %>%
  unnest(value) %>%
  mutate(person_id = row_number(), person_count = 1) %>%
  separate_rows(value, sep = "") %>%
  filter(value != "") %>%
  spread(value, person_count) %>%
  mutate(person_count = 1) %>%
  group_by(group_id) %>%
  summarise_all(sum) %>%
  mutate(person_id = NULL) %>%
  pivot_longer(cols = 2:27, names_to = "questions") %>%
  filter(value == person_count) %>%
  group_by(group_id) %>%
  summarise(n())