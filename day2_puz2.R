## Day 2
##
## Puzzle 2

## Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(glue)

## Read data
passwords <- read_delim(file = "data/day2_puz1_input.txt",
                        delim = " ",
                        col_names = c("positions", "letter",
                                      "password"))

## Clean data
(passwords
  ## Separate letter positions into different (numeric) cols
  %<>% separate(col = positions,
                into = c("pos_1", "pos_2"),
                sep = "-")
  %>% mutate_at(vars(contains("pos")),
                ~ as.integer(.))
  ## Strip colon from letter column
  %>% mutate(letter = str_remove(letter, ":"))
)

## Check whether each password is valid
## and count the number of valid passwords
(passwords
  ## Check for matches in each position
  %>% mutate(letter_pos_1 = 
               letter == str_sub(password,
                                 start = pos_1,
                                 end = pos_1))
  %>% mutate(letter_pos_2 = 
               letter == str_sub(password,
                                 start = pos_2,
                                 end = pos_2))
  ## Count the number of positional matches
  %>% mutate(n_matches = letter_pos_1 + letter_pos_2)
  ## Keep only rows with exactly one positional match
  %>% filter(n_matches == 1) 
  ## Count the number of valid passwords
  %>% nrow(.)
) -> n_valid

## Print the result
print(glue("The number of valid passwords is {n_valid}."))