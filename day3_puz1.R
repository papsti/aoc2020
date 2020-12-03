## Day 3
##
## Puzzle 1

## Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(glue)

## Load data
the_map <- read_delim(file = "data/day3_puz1_input.txt",
                  delim = "\n",
                  col_names = FALSE)

## Get desired number of columns (number of characters in
## each row)
n_col <- nchar(the_map$X1[1])

## Clean data
(the_map
  ## Split each row into separate columns (one character per
  ## column)
  %>% separate(X1,
               into = as.character(1:n_col),
               sep = 1:(n_col-1))
  %>% as.data.frame(.)
  ) -> the_map

## Go down the map and count trees
n_trees <- as.integer(the_map[1,1] == "#") ## check first position
col <- 1 ## column position tracker
col_prev <- 0 ## previous column position tracker

for (row in 2:nrow(the_map)){
  ## starting at row 2 since we already checked [1,1]
  
  ## (tentatively) update column number
  col <- (col + 3) %% (n_col+1)
  ## check whether we've just wrapped around and adjust
  ## for the fact that mod wraps to zero and not 1
  if (col_prev > col){
    col <- col + 1
  }

  ## get entry at current position
  entry <- the_map[row, col]
  ## check for tree
  has_tree <- (entry == "#")
  ## increment tree counter
  n_trees <- n_trees + as.integer(has_tree)
  ## save this col number to check whether
  ## we wrap around next
  col_prev <- col
}

## Print the number of trees
print(glue("You would encounter {n_trees} trees."))
