# Advent of code 2020
# Day 8

# Load libraries

library(tidyverse)

# Set working directory to file location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load input

# Each line represents a code instruction.
# The accumulator starts at 0.
# The accumulator is incremented or decremented via acc instructions, after which execution proceeds to the next instruction.
# nop instructions do nothing regardless of the number given, and execution proceeds to the next instruction.
# jmp instructions make execution proceed with the instruction at the offset given by the number given

input_data <- read_lines("../inputs/day_08")

# Part 1

# Immediately before any instruction would be executed a second time (leading to an infinite loop), what is the value in the accumulator?

# Let's keep it simple this time and just turn the input into a list of lists

program <- str_split(input_data, " ")

# Let's write a function to "execute" the instructions up to the point where they would loop.

run_code_until_loop <- function(code) {

  # Keep track of how many times we've run each instruction
  execution_counts <- vector("list", length(code))

  # Initialize the accumulator
  acc <- 0

  # As much as I normally love avoiding loops in R, I'm giving in today
  n <- 1

  while (is.null(execution_counts[[n]])) {
    this_instruction <- code[[n]][[1]]

    execution_counts[[n]] <- 1

    if (this_instruction == "nop") {
      n <- n + 1
    } else if (this_instruction == "acc") {
      acc <- acc + as.numeric(code[[n]][[2]])
      n <- n + 1
    } else if (this_instruction == "jmp") {
      n <- n + as.numeric(code[[n]][[2]])
    }
  }

  return(acc)
}

print(paste0("The accumulator value is ", run_code_until_loop(program), " before the code gets into an infinite loop."))

# Part 2
# Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp). What is the value of the accumulator after the program terminates?

# OK let's do this one in multiple functions

run_code <- function(code) {

  # Keep track of how many times we've run each instruction
  execution_counts <- rep(0, length(code))

  # Initialize the accumulator
  acc <- 0

  # Keep track of where we are in the function
  pointer <- 1
  path <- list()

  while (TRUE) {
    path <- append(path, pointer)
    
    if (pointer > length(code)) return(list("status" = "done", "path" = path, "result" = acc))

    if (execution_counts[[pointer]] > 0) {
      return(list("status" = "loop", "path" = path, "result" = acc))
    }

    this_instruction <- code[[pointer]][[1]]

    execution_counts[[pointer]] <- execution_counts[[pointer]] + 1

    if (this_instruction == "nop") {
      pointer <- pointer + 1
    } else if (this_instruction == "acc") {
      acc <- acc + as.numeric(code[[pointer]][[2]])
      pointer <- pointer + 1
    } else if (this_instruction == "jmp") {
      pointer <- pointer + as.numeric(code[[pointer]][[2]])
    }
  }

  return(list("status" = "done", "path" = path, "result" = acc))
}

fix_and_run_code <- function(program) {
  first_try <- run_code(program)

  if (first_try[["status"]] == "done") {
    return(first_try[["result"]])
  } else {
    # Again I've given up on not using loops today
    for (instruction in rev(first_try[["path"]])) {
      if(program[[instruction]][[1]] %in% c("nop", "acc")) next
      
      modified_program <- program

      if (modified_program[[instruction]][[1]] == "nop") {
        modified_program[[instruction]][[1]] <- "jmp"
      } else if (modified_program[[instruction]][[1]] == "jmp") {
        modified_program[[instruction]][[1]] <- "nop"
      }

      this_try <- run_code(modified_program)

      if (this_try[["status"]] == "done") {
        return(this_try[["result"]])
      }
    }
  }
}

print(paste0("After fixing the program, the result is ", fix_and_run_code(program), "."))
