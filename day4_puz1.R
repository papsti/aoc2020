## Day 4
##
## Puzzle 1

## Load Libraries
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(glue)

## Read data
passports <- readLines("data/day4_puz1_input.txt")

## Clean data
(passports
  ## replace blank lines with semi-colon
  %<>% str_replace(pattern = "^$", replacement = "\n")
  ## collapse character vector into a single string
  %>% paste(., collapse = " ") 
  %>% read_delim(delim = "\n", col_names = "entry")
)


## Check for valid passports
req_keys <- c("byr", "iyr", "eyr",
              "hgt", "hcl", "ecl",
              "pid")
(passports
  ## Extract entry keys
  %>% mutate(keys = str_extract_all(entry, "[:alpha:]+\\:"))
  ## Remove colon from keys
  %>% mutate(keys = map(keys, ~ str_replace(.x, ":", "")))
  ## Filter for rows where all required keys are present
  %>% filter(map_lgl(keys, ~ all(req_keys %in% .x)))
) -> passports_valid
 
## Count number of valid passports (containing all
## required keys) 
n_valid <- nrow(passports_valid)

print(glue("There are {n_valid} valid passports."))
