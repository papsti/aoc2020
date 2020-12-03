## Day 3
##
## Puzzle 1

## Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
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

## Define function to count trees
count_trees <- function(row_inc, col_inc, the_map){
  
  ## Get number of cols
  n_col <- ncol(the_map)
  
  ## set up list of rows to loop over
  list_of_rows <- seq((1+row_inc), nrow(the_map),
                      by = row_inc)
  
  ## Initialize counters for loop over the rows
  n_trees <- 0 ## stating at 0 because there is no tree
  ## at [1,1]
  col <- 1 ## column position tracker
  col_prev <- 0 ## previous column position tracker
  
  ## Go down the map and count trees
  for (row in list_of_rows){
    ## (tentatively) update column number
    col <- (col + col_inc) %% (n_col+1)
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
  
  return(n_trees)
  
}

## Set up row and col increments to check over
row_inc <- c(rep(1, 4), 2)
col_inc <- c(1, 3, 5, 7, 1)

## Find number of trees for each slope
n_trees <- map2(row_inc, col_inc, count_trees, the_map)

## Take the product of all the tree counts
product <- prod(unlist(n_trees))

## Print the number of trees
print(glue("The product is {product}."))
