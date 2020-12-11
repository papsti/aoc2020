## Day 7
##
## Puzzle 1

## Load libraries
library(readr)
library(tidyr)
library(stringr)
library(dplyr)
library(Matrix)
library(glue)

## Read data
bags <- read_delim("data/day7_puz1_input.txt",
                   delim = "\n",
                   col_names = "line")

## Clean data
(bags 
  %>% separate(line, 
               into = c("bag", "contains"),
               sep = " bags contain ")
  ## strip quantitiy
  %>% mutate(contains = 
               str_replace_all(contains,
                               "[:blank:]?[:digit:]+[:blank:]",
                               ""))
  ## strip " bags."
  %>% mutate(contains = 
               str_replace_all(contains,
                               "[:blank:]?bag[s]?\\.",
                               ""))
  ## replace " bags," with ";" as a separator
  %>% mutate(contains = 
               str_replace_all(contains,
                               "[:blank:]?bag[s]?\\,",
                               ";"))
  ## convert contains character column into list column by
  ## spliting contains string into list of strings
  ## (each list entry is a different bag that's contained)
  %>% mutate(contains = 
               str_split(contains, ";"))
  ## unnest list column into long format
  %>% unnest(contains)
) -> bags

## Create lookup table for matrix indices corresponding to
## each colour
bags_list <- unique(bags$bag)
bag_lookup <- tibble(
  bag = bags_list,
  index = 1:length(bags_list)
)

## Set up empty adjacency matrix as a sparse matrix
adj_mat <- Matrix(data = 0,
                  nrow = length(bags_list),
                  ncol = length(bags_list),
                  sparse = TRUE)

## Attach row and column indices for adjacency matrix to
## bags table
(bags
  ## get row indices
  %>% left_join(bag_lookup, by = "bag")
  ## some convenient renames
  %>% rename(row_index = index, from = bag, bag = contains)
  ## get col indices
  %>% left_join(bag_lookup, by = "bag")
  %>% rename(col_index = index)
  ## drop bag names
  %>% select(row_index, col_index)
  ## drop NAs (corresponding to bags that contain no other)
  %>% drop_na()
) -> edge_list

## Add indicator to adjacency matrix if an edge exists
for (i in 1:nrow(edge_list)){
  row <- edge_list$row_index[i]
  col <- edge_list$col_index[i]
  
  adj_mat[row, col] <- 1
}

## Transpose adjacency matrix to run Markov Chain backward
adj_mat <- t(adj_mat)

## Add identity matrix to avoid nilpotent situation (bags
## which are not contained in any other bags form zero rows)
ident_mat <- Diagonal(nrow(adj_mat))
adj_mat <- ident_mat + adj_mat

## Normalize by row sums to make it a stochastic matrix (and
## so we can avoid overflow error)
adj_mat <- t(scale(t(adj_mat),
                   center = FALSE,
                   scale = rowSums(adj_mat)))

## Convert back to a sparse matrix (scale undoes this)
adj_mat <- Matrix(adj_mat, sparse = TRUE)

## Iterate Markov Chain 20 times (likely not the best way to
## do this since eventually all entries will converge to
## below machine precision... maybe one would use logs to
## avoid underflow?)
stat_mat <- adj_mat
for (i in 1:100){
  stat_mat <- adj_mat %*% stat_mat
}

## Pull out row correpsonding to shiny gold bag which tells
## us the paths the shiny gold bag leads to (i.e. which bags
## eventually contain the shiny gold bag) but exclude the
## diagonal element, which represents the shiny gold bag
## being contained by itself.
## This edge was only included so that the Markov
## Chain doesn't leak out mass (source bags that aren't
## contained in others would cause mass to leak because they
## would be represented by zero rows in the adjacency
## matrix))
shiny_gold_index <- (bag_lookup
                     %>% filter(bag == "shiny gold")
                     %>% pull(index)
)
to_shiny_gold <- stat_mat[shiny_gold_index, -shiny_gold_index]

## Isolate entries where there is non-zero mass
to_shiny_gold <- case_when(
  to_shiny_gold != 0 ~ TRUE,
  TRUE ~ FALSE)

## Count number of bags that eventually contain a shiny gold bag
print(glue("Number of bag colours eventually containing at least one shiny gold bag: {sum(to_shiny_gold)}"))
