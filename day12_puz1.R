## Day 12
## 
## Puzzle 1

## Load libraries
library(readr)
library(stringr)
library(dplyr)
library(glue)

## Read data
directions <- read_delim("data/day12_puz1_input.txt",
                         delim = "\n",
                         col_names = "move")

## Clean data
(directions 
  ## split up actions and values
  %>% mutate(action = str_extract(move,
                                  "[:alpha:]"))
  %>% mutate(value = as.numeric(
    str_extract(move,
                "[:digit:]+")
  ))
  ## convert N/S/E/W direction to signed vert or horiz moves
  %>% mutate(value = case_when(
    action == "S" | action == "W" ~ -value,
    T ~ value
  ))
  %>% mutate(action = case_when(
    action == "N" | action == "S" ~ "vert",
    action == "E" | action == "W" ~ "horiz",
    T ~ action
  ))
  ## convert L/R direction to signed angle
  %>% mutate(value = case_when(
    action == "R" ~ -value,
    T ~ value
  ))
  %>% mutate(action = case_when(
    action == "L" | action == "R" ~ "angle",
    T ~ action
  ))
  ## get rid of original move column
  %>% select(-move)
) -> directions

## Set up initial position list
pos <- list(vert = 0,
            horiz = 0,
            angle = 0)

## Function to update position given direction
update_pos <- function(pos, direction){
  ## pos is a list with elements
    ## vert: vertical position
    ## horiz: horizontal position
    ## angle: facing direction
  ## direction is an instruction from the directions df,
  ## as parsed above (one row of the df)
  
  ## if action is "F", preprocess this
  if(direction$action == "F"){
    
    ## if turned W or S, make value negative
    if(pos$angle %in% c(180, 270)){
      direction$value <- -direction$value
    }
    
    ## update action based on current angle
    direction$action <- case_when(
      pos$angle %in% c(0, 180) ~ "horiz",
      T ~ "vert"
    )
  }
  
  ## update position
  pos[direction$action] <- pos[[direction$action]] + direction$value
   
  ## map angle back to [0, 365) (just in case the above
  ## update moved us out of this interval)
  pos$angle <- pos$angle %% 360
  
  return(pos)
}

## Iterate through directions and update position at each
## step
for (row in 1:nrow(directions)){
  pos <- update_pos(pos, directions %>% slice(row))
}

print(glue("final manhattan distance from start: {abs(pos$vert)+abs(pos$horiz)}"))
