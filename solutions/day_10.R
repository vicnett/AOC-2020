# Advent of code 2020
# Day 10

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

# Input is a list of integers.
# Each integer represents the "input joltage" rating of a power adapter.
# Each power adapter can accept joltage betweeen 1 and 3 "jolts" below its rating

# Not represented in the data:
# A mobile device with a "joltage" rating 3 jolts greater than the highest rating of all adapters.
# The wall outlet, rated at zero "jolts".

input_data <- as.numeric(read_lines("../inputs/day_10"))
sample_input_data <- as.numeric(read_lines("28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"))

# Part 1

# Come up with a way to use ALL the power adapters to charge the mobile device.
# Calculate the difference in "joltage" in each stage of the chain.
# Count how many 1-jolt and 3-jolt differences there are, and multiply the two numbers together.

# First let's add the wall outlet and the final device to the list
joltage <- sort(c(0, input_data, max(input_data) + 3))

# Sort them, and calculate the power differences between each of them
power_adapter_chain <- tibble(joltage) %>%
  mutate(difference = joltage - lag(joltage))

part_1_answer <- power_adapter_chain %>%
  filter(!is.na(difference)) %>%
  group_by(difference) %>%
  summarise(count = n()) %>%
  `[[`("count") %>%
  prod()

print(paste0("The number of 1-jolt differences multiplied by the number of 3-jolt differences in the chain of all power adapters is ", part_1_answer, "."))

# Part 2

# How many different arrangements of power adapters are there to connect the device to the wall outlet?
# No need to use them all in each combination, but otherwise the same rules apply

count_possible_paths <- function(ordered_joltage_list) {
  # We take a SORTED list of adapters
  possible_paths <- tibble(joltage = ordered_joltage_list) %>%
    # Start with the assumption that there is always one path. We know this because part 1 shows we can make a chain using every single adapter.
    mutate(possible_paths_so_far = 1)
  
  for (i in 1:nrow(possible_paths)) {
    # Now, at each adapter in the sorted list, we check the previous three adapters to see if they are compatible.
    # If all three are compatible, then the number of possible combinations at this point is the sum of combinations possible at
    # these three previous points. Same for the previous two, and the default is one, where we keep the same number as before as we are
    # not adding any more possible combinations.
    possible_paths$possible_paths_so_far[[i]] <-
      if_else(
        possible_paths$joltage[[i]] - lag(possible_paths$joltage, 3, 0)[[i]] == 3,
        lag(possible_paths$possible_paths_so_far, 1, 0)[[i]] + lag(possible_paths$possible_paths_so_far, 2, 0)[[i]] + lag(possible_paths$possible_paths_so_far, 3, 0)[[i]],
        if_else(
          possible_paths$joltage[[i]] - lag(possible_paths$joltage, 2, 0)[[i]] == 2,
          lag(possible_paths$possible_paths_so_far, 1, 0)[[i]] + lag(possible_paths$possible_paths_so_far, 2, 0)[[i]],
          lag(possible_paths$possible_paths_so_far, 1, 1)[[i]]
        )
      )
    
  }
  
  return(possible_paths)
  
}

possible_path_counts <- count_possible_paths(joltage)

part_2_answer <- max(possible_path_counts$possible_paths_so_far)

print(paste0("There are a total of ", part_2_answer, " possible combinations of power adapters."))
