# Advent of code 2020
# Day 7

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

# Each line lists the contents allowable inside a certain type of bag.
# It will either state what other bags can be inside the containing bag, or state that it contains "no other bags"
# Example below

inputData <- read_lines("../inputs/day_07")
sampleInputData <- read_lines("light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

# Part 1
# How many bag colors can eventually contain at least one shiny gold bag?

# First let's get this data in to a usable state

bagRules <- tibble(raw_bag_definition = inputData) %>%
  # We can clearly separate the name of the bag from the description of its contents
  separate(
    raw_bag_definition,
    into = c("bag_name", "bag_contents"),
    sep = " bags contain "
  ) %>%
  # Let's say an empty bag has NA contents, and remove the trailing period
  mutate(bag_contents = na_if(str_replace(bag_contents, "\\.", ""), "no other bags.")) %>%
  # Split the contents so that we have one bag type pair per row
  mutate(contained_bag = str_split(bag_contents, ", ")) %>%
  unnest(contained_bag) %>%
  # Break out the quantity into its own column and remove the trailing word "bag/bags"
  mutate(contained_bag_qty = as.numeric(str_extract(contained_bag, "^[:digit:]+")), contained_bag = str_replace_all(contained_bag, "^[:digit:]+ | bags?", "")) %>%
  replace_na(list(contained_bag_qty = 0)) %>%
  # We don't need the original "contents" string anymore
  select(-bag_contents)

# Now let's write a function to list out all possible containers for a given bag

listAllContainers <- function(bag_name, bag_rules) {
  
  # Find all "immediate" containers
  
  container_list <- bag_rules$bag_name[which(bag_rules$contained_bag == bag_name)]
  
  # And then rinse/repeat and put all the results together
  
  return(unique(unlist(append(container_list, lapply(container_list, listAllContainers, bag_rules)))))
  
}

print(paste0("There are ", length(listAllContainers("shiny gold", bagRules)), " different bags which can eventually contain at least one shiny gold bag."))

# Part 2
# How many individual bags are required inside a single shiny gold bag?

# Let's write another function to list all bags contained within a given bag

countAllContainedBags <- function(outer_bag_name, bag_rules) {
  
  # Find all "immediate" contained bags
  
  contained_list <- bag_rules[which(bag_rules$bag_name == outer_bag_name), ]
  
  # Then rinse and repeat again til we have them all
  
  # Except we need to tell it when to stop this time... In this case, when all contained bags found cannot contain any more bags.
  if(sum(contained_list$contained_bag_qty) == 0) {
    
    return(0)
    
  } else {
    
    expanded_list <- contained_list %>%
      uncount(contained_bag_qty)
    
    return(sum(contained_list$contained_bag_qty) + do.call(sum, lapply(expanded_list$contained_bag, countAllContainedBags, bag_rules)))
    
  }
  
}

print(paste0("A shiny gold bag must contain ", countAllContainedBags("shiny gold", bagRules), " bags in total."))
