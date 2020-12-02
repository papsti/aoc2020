## Day 1
## 
## Puzzle 1

## Load libraries
library(readr)
library(dplyr)
library(glue)

## Read data
expenses <- read_delim(file = "data/day1_puz1_input.txt",
                       delim = "\n",
                       col_names = "value")

expenses %>%
  ## calculate difference between 2020 and all values, 
  ## to determine target value in each case
  mutate(diff = 2020-value) %>%
  ## filter for observations where the diff is found in
  ## original value
  filter(diff %in% value) %>%
  ## calculate product
  mutate(product = value*diff) %>%
  ## pull single product value (there will be two
  ## corresponding to the same value)
  distinct(product) %>%
  pull() -> the_product
  
## Print result
print(glue("found it! the product is {the_product}."))