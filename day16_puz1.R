## Day 16
##
## Puzzle 1

## Load libraries
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(glue)

## Read data
rawdata <- read_delim("data/day16_puz1_input.txt",
                      delim = "\n",
                      col_names = "line")

## TIDY DATA
###############

## Rules
(rawdata[1:20,] 
  ## separate field rules into nicely parsed columns
  %>% separate(line,
               into = c("field", "lims"),
               sep = ": ")
  %>% separate(lims,
               into = c("lim1", "lim2"),
               sep = " or ")
  %>% separate(lim1,
               into = c("lim1_min", "lim1_max"),
               sep = "-")
  %>% separate(lim2,
               into = c("lim2_min", "lim2_max"),
               sep = "-")
  ## convert limits to numeric
  %>% mutate(across(starts_with("lim"), as.numeric))
  ## convert to long format so that we only have one min and
  ## max column each
  %>% pivot_longer(starts_with("lim"),
                   names_to = c("lim", ".value"),
                   names_pattern = "(.+)_(.+)")
  %>% select(-lim)
) -> rules

## Tickets
(rawdata[24:nrow(rawdata),]
  %>% transmute(values = strsplit(line, split = ","))
  %>% mutate(values = map(values, as.numeric))
) -> tickets

## CHECK TICKETS
###################

## Function to check a single ticket value against all of
## the rules and let us know if there is *at least one* rule
## satisfied by the value
check_value <- function(value, rules){
  
  (rules
   ## check focal value against min and max of each range
   %>% mutate(check = (value >= min & value <= max))
   ## ensure the value fits at least one rule
   %>% summarise(any_true = any(check))
   %>% pull(any_true)
  ) -> check
  
  return(check)
}

## Flag invalid tickets (at least one invalid value)
(tickets
  ## check each value in the list of values against the
  ## rules 
  ## TODO: this double map is kinda slow... is there a
  ## more efficient solution?
  %>% mutate(is_valid_entry = map(values, ~map_lgl(., check_value, rules = rules)))
  ## check whether all ticket values are valid
  %>% mutate(is_valid_ticket = map_lgl(is_valid_entry, all))
  ##get values corresponding to invalid entries
  %>% mutate(invalid_values = map2(values, is_valid_entry, function(x,y) x[!y]))
) -> tickets

## Get ticket scanning error rate
err_rate <- sum(unlist(tickets$invalid_values))

print(glue("the ticket error rate is {err_rate}"))