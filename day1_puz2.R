## Day 1
## 
## Puzzle 2

## Load libraries
library(readr)
library(dplyr)
library(glue)

## Read data
expenses <- read_delim(file = "data/day1_puz1_input.txt",
                       delim = "\n",
                       col_names = "value")

expenses %>%
  ## calculate difference between 2020 and all values, 
  ## to determine a target sum in each case
  mutate(diff = 2020-value) -> expenses

for (i in 1:nrow(expenses)){
  ## grab a focal row (x values)
  focal_row <- expenses[i,]
  
  ## find candidate rows that could have a y such that 
  ## x + y + z = 2020
  ## in either case, y and z both have to be <= 2020 - x
  expenses[-i,] %>%
    filter(value <= focal_row$diff) -> candidate_rows
  
  ## if there are actually exist other rows satisfying 
  ## y <= 2020 - x
  if(nrow(candidate_rows) > 0){
    candidate_rows %>%
      ## calcuate 2020 - x - y = z
      mutate(diff_2 = focal_row$diff - value) %>%
      ## check if we have a matching z in our candidate
      ## values
      filter(value %in% diff_2) %>%
      ## pull just the value col
      pull(value) -> solution
    
    ## if we have a solution, break out of the for loop
    if(length(solution) > 0){
      break
    }
  }
}

## calculate the product of x, y, and z and print
product <- focal_row$value*solution[1]*solution[2]
print(glue("found it! the product is {product}."))
