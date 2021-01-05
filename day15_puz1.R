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

for (turn in next_turn:max_turn){
  last_spoken <- record$spoken[turn-1]
  
  (record 
    %>% filter(spoken == last_spoken)) -> record_subset
  
  (record_subset
    %>% nrow(.)) -> n_spoken
  
  if(n_spoken == 1){
    spoken <- 0
  } else {
    (record_subset
      %>% arrange(desc(turn))) -> record_rev
    
    turn_diff <- record_rev$turn[1]-record_rev$turn[2]  
    
    spoken <- turn_diff
  }
  
  ## udpate record
  record <- bind_rows(record,
                      tibble(turn = turn, spoken = spoken))
  
}

print(glue("the {max_turn}th number spoken is {record$spoken[max_turn]}"))
