## Day 6
##
## Puzzle 2

## Load libraries
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(glue)

## Helper functions
######################

## Define function to drop blank entry from list of
## strings
drop_blanks <- function(list_of_strings){
  ## Get indices of non-blank entries
  keep_indx <- which(list_of_strings[[1]] != "")
  ## Keep those
  list(list_of_strings[[1]][keep_indx])
}

get_common_letters <- function(list_of_strings){
  ## Split each string into list of characters
  list_of_characters <- unlist(lapply(list_of_strings,
                                      strsplit,
                                      split = ""),
                               recursive = FALSE)
  ## Find letters common to all strings
  common_characters <- Reduce(intersect, list_of_characters)
  
  return(list(common_characters))
}

## Process data
#################

## Read and clean data
customs <- (
  readLines("data/day6_puz1_input.txt")
  ## replace blank lines with semi-colon
  %>% str_replace(pattern = "^$", replacement = "\n")
  ## collapse character vector into a single string with
  ## semi-colons separating
  %>% paste(., collapse = ";")
  %>% read_delim(delim = "\n", col_names = "answers")
  ## parse answers into character lists
  %>% rowwise()
  %>% mutate(list_of_answers = str_split(answers, ";"))
  ## drop blank answer entries
  %>% mutate(list_of_answers = lmap(list(list_of_answers),
                                    drop_blanks))
) -> customs

## Get total number questions where the entire group
## answered 'yes'
(customs
  ## get common letters
  %>% mutate(common_letters = lmap(list(list_of_answers),
                                   get_common_letters))
  ## count number of common letters per group
  %>% mutate(n_common_letters = length(common_letters))
  ## get total number of answers where the whole group answered 'yes'
  %>% ungroup()
  %>% select(n_common_letters)
  %>% sum(.)
) -> sum_counts

print(glue("Total count of questions where the entire group answered 'yes' is {sum_counts}."))
