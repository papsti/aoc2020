## Day 14
## 
## Puzzle 1

## Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(binaryLogic)
library(glue)

## Load lines of data
data <- read_lines("data/day14_puz1_input.txt")

## Reshape data into tidy form
df <- tibble(
  mask = character(0),
  line = character(0)
)

current_mask <- data[1]

for (i in 2:length(data)){
  line_start <- substr(data[i], start = 1, stop = 4)
  if(line_start == "mask"){
    ## if it's a mask line, just updated the current mask
    ## and go to next iteration
    current_mask <- data[i]
    next
  }
  ## get the current line
  current_line <- data[i]
  
  ## write line into df
  df <- bind_rows(df, tibble(
    mask = current_mask,
    line = current_line
  ))
}

## Clean up cols in data
(df
  %>% mutate(mask = str_extract(mask, "[X01]+"))
  %>% separate(line, into = c("loc", "value"),
               sep = " = ")
  %>% mutate(loc = as.numeric(str_extract(
    loc, "[:digit:]+"
  )))
  %>% mutate(value = as.numeric(value))
  ## keep only the last update to each location
  %>% map_df(rev) ## reverse order of data frame rows
  %>% distinct(loc, .keep_all = TRUE) ## keep last obs of loc
  ## (last in original df order)
) -> df

## Function to apply mask to instruction value (mutate into new col)
apply_mask <- function(mask, value){
  ## Convert value to 36 digit binary
  value <- as.binary(value, n = 36)
  value <- as.character(value)
  
  ## Split mask up into character vector
  mask <- strsplit(mask, "")[[1]]
  
  ## Make substitutions to value based on mask
  for (i in 1:length(value)){
    if(mask[i] == "X"){
      next
    }
    value[i] <- mask[i]
  }
  
  ## Return masked integer
  int_value <- sum(2^{35:0}*as.numeric(value))
  
  return(int_value)
}

## Get sum of masked values
(df
  ## Mask values
  %>% rowwise()
  %>% mutate(masked_values = apply_mask(mask, value))
  %>% ungroup()
  ## Sum masked values
  %>% summarise(sum_masked = sum(masked_values))
  %>% pull(sum_masked)
) -> sum_masked

print(glue("sum of masked values: {sum_masked}"))