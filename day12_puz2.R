## Day 12
##
## Puzzle 2

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
## c(horiz, vert)
ship <- c(0,0)
waypoint <- c(10,1)

pos <- matrix(data = c(ship, waypoint),
              nrow = 2)

## Function to update position given direction
update_pos <- function(pos,
                       direction){
  ## pos is a matrix where the cols represent the ship and
  ## waypoint while the rows are horizontal and
  ## vertical positions
  ## for ship, relative to starting position of (0,0)
  ## for waypoint, pos relative to ship's current pos
  ##
  ## direction is an instruction from the
  ## directions df, as parsed above (one row of the df)
  
  action <- direction$action
  value  <- direction$value
  
  ## if action is "F", move the ship toward the waypoint
  if(action == "F"){
    pos[,1] <- pos[,1] + value * pos[,2]
  } else if (action == "angle") { ## rotate the waypoint
    theta <- value*pi/180 ## convert to radians
    rot_mat <- round(matrix(data = c(cos(theta), sin(theta),
                               -sin(theta), cos(theta)),
                            nrow = 2))
    pos[,2] <- rot_mat %*% pos[,2]
  } else if (action == "vert"){ ## move the waypoint
    pos[2,2] <- pos[2,2] + value
  } else if (action == "horiz"){
    pos[1,2] <- pos[1,2] + value
  } else {
    stop("direction not recognized. action must be either 'horiz', 'vert', 'angle', or 'F'")
  }
  
  return(pos)
}

## Iterate through directions and update position at each
## step
for (row in 1:nrow(directions)){
  pos <- update_pos(pos, directions %>% slice(row))
}

print(glue("final manhattan distance from start: {abs(pos[1,1])+abs(pos[2,1])}"))
