# Advent of code 2020
# Day 9

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

# Input is a list of integers.
# Each integer is supposed to be the sum of two integers within the previous n integers.
# The first n integers in the list are the preamble, necessary for "seeding" the list with numbers to sum.

input_data <- as.numeric(read_lines("../inputs/day_09"))
sample_input_data <- as.numeric(read_lines("35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"))

# Part 1

# Find the first number in the list that is NOT the sum of one of the 25 previous ones

# Let's write a function to process the input with an arbitrary preamble length

process_xmas_cypher <- function(data_stream, preamble_length = 25) {
  data_value <- data_stream[(preamble_length + 1):length(data_stream)]
  sum_bucket <- lapply((preamble_length + 1):length(data_stream), function(x) data_stream[(x - preamble_length):x])
  possible_sums <- lapply(sum_bucket, function(x) unlist(lapply(combn(x, 2, simplify = FALSE), sum)))
  is_valid <- lapply(seq_len(length(data_value)), function(x) data_value[[x]] %in% possible_sums[[x]])

  processed_cypher <- tibble(
    data_value,
    sum_bucket,
    possible_sums,
    is_valid
  )
}

processed_input_data <- process_xmas_cypher(input_data)

part_1_answer <- processed_input_data$data_value[which(processed_input_data$is_valid == FALSE)][[1]]

print(paste0("The first value that does not fulfill the requirements is ", part_1_answer, "."))

# Part 2

# Find a contiguous set of at least two numbers in the list that add up to the invalid number from Part 1
# Add together the smallest and largest numbers in that set.

# I guess I really need a function to find sums, eh? ¬_¬

find_sums <- function(numbers_list, target_sum, n_min = 2, n_max = Inf, must_be_contiguous = FALSE) {

  if (must_be_contiguous) {
    # Generate a list of all possible subsets of the list within the range of lengths required
    combos <- do.call(c, lapply(n_min:min(n_max, length(numbers_list)), function(x) lapply(1:(length(numbers_list) - x), function(y) numbers_list[y:(y + x)])))
  } else {
    # First of all, throw out all numbers that are bigger than the target since we don't care about order here
    trimmed_list <- numbers_list[which(numbers_list < target_sum)]
    # Find all possible combinations of numbers in the list, made of as many numbers as required
    combos <- do.call(c, lapply(n_min:min(n_max, length(trimmed_list)), function(x) combn(trimmed_list, x, simplify = FALSE)))
  }
  
  combo_sums <- lapply(combos, sum)
  return(combos[which(combo_sums == target_sum)])
}

contiguous_range_that_sums_to_the_part_1_answer_yes_i_have_given_up_on_coming_up_with_names_how_can_you_tell <- find_sums(input_data, part_1_answer, must_be_contiguous = TRUE)[[1]]

part_2_answer <- min(contiguous_range_that_sums_to_the_part_1_answer_yes_i_have_given_up_on_coming_up_with_names_how_can_you_tell + max(contiguous_range_that_sums_to_the_part_1_answer_yes_i_have_given_up_on_coming_up_with_names_how_can_you_tell))

print(paste0("The sum of the smallest and largest numbers in the contiguous range of numbers in the input which sums up to ", part_1_answer, " is ", part_2_answer, "."))