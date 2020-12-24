## Day 9
##
## Puzzle 1

## Load libraries
library(readr)
library(dplyr)
library(glue)

## Read data
df <- read_delim("data/day9_puz1_input.txt",
                 delim = "\n",
                 col_names = "value") 

## Define function to check whether a value is valid
check_valid <- function(df, index){
  ## grab the preamble
  preamble <- df %>% slice((index-25):(index-1))
  
  ## grab the target value
  target <- df %>% slice(index) %>% pull(value)
  
  ## get difference between target value and all values in
  ## preamble
  difference <- target-preamble
  
  ## get number of matches
  matches <- intersect(difference, preamble)
  multiple_matches <- nrow(matches) > 0
  
  ## check that matches are distinct
  is_distinct <- nrow(distinct(matches)) > 1
  
  if(multiple_matches & is_distinct){
    is_valid <- TRUE
  } else {
    is_valid <- FALSE
  }
  
  return(is_valid)
}

## loop over values
for (i in 26:nrow(df)){
  ## check if entry is valid
  is_valid <- check_valid(df, i)
  
  if(!is_valid){
    print(glue("first number that is not valid: {df$value[i]}"))
    break
  }
}
