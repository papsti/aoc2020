## Day 15
##
## Puzzle 1

## Load libraries
library(tibble)
library(dplyr)
library(glue)

## Read data
starting_numbers <- c(6,19,0,5,7,13,1)

## Set up record of spoken numbers
record <- tibble(
  turn = 1:length(starting_numbers),
  spoken = starting_numbers
)

next_turn <- nrow(record)+1
max_turn <- 2020

## Iterate over turns and update record using the stated
## rules
for (turn in next_turn:max_turn){
  ## Get the last spoken number
  last_spoken <- record$spoken[turn-1]
  
  ## Get records featuring last spoken number
  (record 
    %>% filter(spoken == last_spoken)) -> record_subset
  
  ## Count number of times last spoken number was recorded
  (record_subset
    %>% nrow(.)) -> n_spoken
  
  ## If only spoken once, return 0 (this is the first
  ## mention)
  if(n_spoken == 1){
    spoken <- 0
  } else {## Otherwise, get diff of turn numbers from last
          ## two mentions
    (record_subset
      %>% arrange(desc(turn))) -> record_rev
    
    turn_diff <- record_rev$turn[1]-record_rev$turn[2]  
    
    spoken <- turn_diff
  }
  
  ## Update record
  record <- bind_rows(record,
                      tibble(turn = turn, spoken = spoken))
  
}

## Print number spoken on the last turn
print(glue("the {max_turn}th number spoken is {record$spoken[max_turn]}"))
