# Advent of code 2020
# Day 4

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

inputData <- read_file("../inputs/day_04")

# Part 1
# Input is "passport" data.
# Determine which passports have all required fields. The expected fields are as follows:
#  - byr (Birth Year)
#  - iyr (Issue Year)
#  - eyr (Expiration Year)
#  - hgt (Height)
#  - hcl (Hair Color)
#  - ecl (Eye Color)
#  - pid (Passport ID)
#  - cid (Country ID)
# The cid field is optional
# Output should be the number of valid passports

# Preprocess input

passportList <- str_replace_all(str_split(inputData, '\\n\\n')[[1]], "\\n", " ") %>%
  as_tibble() %>%
  mutate(
    # Create an "id" column to keep track of which passport each data point belongs to
    id = row_number(),
    # And split the strings into key/value pairs
    value = str_split(value, " ")
  ) %>%
  # Put all the key/value pairs into one column
  unnest(value) %>%
  # Separate the key/value pairs into a key and a value columns
  separate(value, into = c("key", "value"), sep = ":") %>%
  # Remove invalid rows (key and/or value missing)
  drop_na() %>%
  # Move the keys and values into columns
  spread(key, value) %>%
  # Replace missing country IDs since we don't care if it's missing
  replace_na(list(cid = "none"))

# The answer is simply how many rows have no missing values!

print(paste0("The list contains ", length(drop_na(passportList)$id), " valid passports."))

# Part 2
# Count all valid passports, same as part 1, with the following data validation rules added:
#   - byr (Birth Year) - four digits; at least 1920 and at most 2002.
#   - iyr (Issue Year) - four digits; at least 2010 and at most 2020.
#   - eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
#   - hgt (Height) - a number followed by either cm or in:
#     - If cm, the number must be at least 150 and at most 193.
#     - If in, the number must be at least 59 and at most 76.
#   - hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
#   - ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
#   - pid (Passport ID) - a nine-digit number, including leading zeroes.
#   - cid (Country ID) - ignored, missing or not.

passportListPassingDataValidation <- passportList %>%
  # Filter out bad byr values
  filter(
    str_detect(byr, "^[:digit:]{4,4}$"),
    between(as.numeric(byr), 1920, 2002)
  ) %>%
  # Filter out bad iyr values
  filter(
    str_detect(iyr, "^[:digit:]{4,4}$"),
    between(as.numeric(iyr), 2010, 2020)
  ) %>%
  # Filter out bad eyr values
  filter(
    str_detect(eyr, "^[:digit:]{4,4}$"),
    between(as.numeric(eyr), 2020, 2030)
  ) %>%
  # Filter out bad hgt values
  # First let's separate the number and the unit
  mutate(
    hgt_unit = str_sub(hgt, -2, -1),
    hgt_number = as.numeric(str_sub(hgt, 1, -3))
  ) %>%
  filter(
    (hgt_unit == "cm" & between(hgt_number, 150, 193)) |
    (hgt_unit == "in" & between(hgt_number, 59, 76))
  ) %>%
  # Filter out bad hcl values
  filter(
    str_detect(hcl, "^#[a-f0-9]{6,6}")
  ) %>%
  # Filter out bad ecl values
  filter(ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>%
  # Filter out bad pid values
  filter(
    str_detect(pid, "^[:digit:]{9,9}$")
  )

print(paste0("The list contains ", length(passportListPassingDataValidation$id), " valid passports, including data validation."))
