## Day 2
##
## Puzzle 1

## Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(glue)

## Read data
passwords <- read_delim(file = "data/day2_puz1_input.txt",
                        delim = " ",
                        col_names = c("bounds", "letter",
                                      "password"))

## Clean data
(passwords
  ## Separate letter bounds into different (numeric) cols
  %<>% separate(col = bounds,
               into = c("bounds_min", "bounds_max"),
               sep = "-",
               convert = TRUE)
  ## Strip colon from letter column
  %>% mutate(letter = str_remove(letter, ":"))
)

## Check whether each password is valid
## and count the number of valid passwords
(passwords
  ## Count number of instances of the focal letter in each
  ## password
  %>% mutate(letter_count = str_count(password, letter))
  ## Is the count of the focal letter within the specified
  ## bounds?
  %>% mutate(is_valid = case_when(
     letter_count >= bounds_min & letter_count <= bounds_max ~ TRUE,
     T ~ FALSE
   ))
  ## Count the number of valid passwords
  %>% summarise(n_valid = sum(is_valid))
  %>% pull(n_valid)
) -> n_valid

## Print the result
print(glue("The number of valid passwords is {n_valid}."))