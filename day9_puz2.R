## Day 9
##
## Puzzle 2

## Load libraries
library(readr)
library(dplyr)
library(glue)

## Read data
df <- read_delim("data/day9_puz1_input.txt",
                 delim = "\n",
                 col_names = "value") 

## Set target value
target <- 31161678

## Filter data to be less than target
(df 
  %>% filter(value < target)
  )

stop <- FALSE
for (start in 1:nrow(df)){
  for (end in (start+1):nrow(df)){
    ## subset df
    (df
     %>% slice(start:end)
     %>% pull(value)
     ) -> subset
    
    ## if we've  reached our target, print it and break out
    ## of both loops
    if (sum(subset) == target){
      the_min <- min(subset)
      the_max <- max(subset)
      print(glue("the encryption weakness is {the_min + the_max}"))
      stop <- TRUE
      break
    }
    
    ## if we've already exceeded the target value, go to the
    ## next start value
    if (sum(subset) > target){
      break
    }
  }
  if (stop){
    break
  }
}