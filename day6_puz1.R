## Day 6
##
## Puzzle 1

## Load libraries
library(dplyr)
library(stringr)
library(readr)
library(glue)

## Read and clean data
customs <- (readLines("data/day6_puz1_input.txt")
  ## replace blank lines with semi-colon
  %>% str_replace(pattern = "^$", replacement = "\n")
  ## collapse character vector into a single string
  %>% paste(., collapse = "") 
  %>% read_delim(delim = "\n", col_names = "answers")            
)

## Define helper function to get unique characters from a
## string
count_unique_characters <- function(string){
  ## Convert string to list of characters
  string_list <- strsplit(string, split = "")[[1]]
  ## Get distinct characters in the list
  string_list <- unique(string_list)
  ## Count number of unique characters in the string
  n_unique <- length(string_list)
  
  return(n_unique)
}

## Count number of questions where at least one group member
## answered yes
(customs
  %>% rowwise()
  %>% mutate(unique_answers =
               count_unique_characters(answers))
) -> customs

## Find sum of all positives
(customs
  %>% select(unique_answers)
  %>% sum(.)
) -> sum_positives

print(glue("The total of questions to which anyone answered 'yes' is {sum_positives}."))
