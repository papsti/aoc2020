## Day 10 
##
## Puzzle 1

## Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(glue)

## Read data
df <- read_delim("data/day10_puz1_input.txt",
                 delim = "\n",
                 col_names = "jolts") 

(df
  ## Add rows for first and last value
  %>% bind_rows(tibble(jolts = 0),
                .,
                tibble(jolts = max(df$jolts)+3))
  ## sort and get first differences
  %>% arrange(jolts)
  %>% summarise(diff = diff(jolts))
  ## count number of 1- and 3-jolt diffs
  %>% group_by(diff)
  %>% count()
  ## get product of 1- and 3-jolt diffs
  %>% pivot_wider(names_from = diff,
                  values_from = n)
  %>% mutate(prod = `1`*`3`)
  %>% pull(prod)
) -> prod

print(glue("the number of 1-jolt diffs times the number of 3-jolt diffs is {prod}"))
