## Day 8
##
## Puzzle 1

## Load libraries
library(readr)
library(tidyr)
library(dplyr)
library(glue)

## Read data
instructions <- read_delim("data/day8_puz1_input.txt",
                           delim = "\n",
                           col_names = "line")

## Tidy data
(instructions
  %>%
    separate(line,
             into = c("directive", "increment"),
             sep = " "
             )
  %>%
    mutate(increment = as.numeric(increment))
  ) -> instructions

## Step through instructions
continue <- TRUE
accumulator <- 0
current_row <- 1
visited_rows <- numeric(0)

while(continue){
  ## check current row hasn't previously been visited
  if (current_row %in% visited_rows){
    print(current_row)
    break
  }
  
  ## add current row to visited rows
  visited_rows <- c(visited_rows, current_row)
  
  ## check and perform instruction
  inst <- instructions %>% slice(current_row)
  
  if (inst$directive == "acc"){
    accumulator <- accumulator + inst$increment
  }
  
  jump <- ifelse(inst$directive == "jmp", inst$increment, 1)
  current_row <- current_row + jump
}

## Report value of accumulator
glue("accumulator value: {accumulator}")