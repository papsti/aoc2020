## Day 5
##
## Puzzle 1

## Load libraries
library(glue)
library(stringr)
library(dplyr)
library(readr)

## Define helper functions
#############################

## Define function to update intervalusing
## front or back designation and current interval
update_interval <- function(front_or_back, interval){
  ## Check that inputs are valid
  if(front_or_back != "F" & front_or_back != "B"){
    stop('front_or_back must be either "F" or "B"')
  }
  
  if(length(interval) != 2){
    stop('interval must be of length 2')
  }
  
  ## calculate midpoint
  midpoint <- sum(c(interval, 1))/2-1
  
  ## update interval
  if(front_or_back == "F"){
    interval <- c(interval[1],
                  midpoint)
  } else {
    interval <- c(midpoint + 1,
                  interval[2])
  }
  
  return(interval)
}

## Get row/col number from row/col string
get_number <- function(string, interval){
  ## interval: start interval
  ## (either [0, 127] for rows or [0, 7] for cols)
  
  ## Get list of row/col letters
  letters <- strsplit(string, "")[[1]]
  
  ## Loop over row letters
  for (letter in letters){
    interval <- update_interval(letter, interval)
  } 
  
  ## Get row number
  return(interval[1])
}

## Process data
#################

## Initialize start intervals for rows and cols
row_interval <- c(0, 127)
col_interval <- c(0, 7)

seats <- (read_delim("data/day5_puz1_input.txt",
                    delim = "\n",
                    col_names = "seat_string")
  ## replace L with F and R with B to standardize so that we
  ## can use the same update_interval function for both row
  ## and col
  %>% mutate(seat_string = str_replace_all(seat_string,
                                           "L", "F"))
  %>% mutate(seat_string = str_replace_all(seat_string,
                                           "R", "B"))
  ## pull out substrings corresponding to rows and cols
  %>% mutate(row_string = str_sub(seat_string,
                                  start = 1, end = 7))
  %>% mutate(col_string = str_sub(seat_string,
                                  start = 8, end = 10))
  ## get row and col numbers
  %>% rowwise()
  %>% mutate(row_number = get_number(row_string,
                                     row_interval))
  %>% mutate(col_number = get_number(col_string,
                                     col_interval))
  ## get seat ID
  %>% mutate(seat_id = row_number*8 + col_number)
  ## get max seat ID
  %>% ungroup()
)

(seats
  %>% select(seat_id)
  %>% max(.)
) -> max_seat_id

print(glue("The highest seat ID is {max_seat_id}."))
