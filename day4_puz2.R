## Day 4
##
## Puzzle 2

## Get valid passports (and load libraries)
source("day4_puz1.R")

## Load other libraries
library(tidyr)

## Clean data
(passports_valid
  ## Make columns for each variable
  %<>% mutate(byr = as.integer(str_replace(str_extract(
    entry, "byr\\:[:digit:]+[:blank:]?" ## find entry
  ), "byr\\:", ""))) ## remove key prefix
  %>% mutate(iyr = as.integer(str_replace(str_extract(
    entry, "iyr\\:[:digit:]+[:blank:]?"
  ), "iyr\\:", "")))
  %>% mutate(eyr = as.integer(str_replace(str_extract(
    entry, "eyr\\:[:digit:]+[:blank:]?"
  ), "eyr\\:", "")))  
  %>% mutate(hgt_val = as.integer(str_replace(
    str_extract(
    entry, "hgt\\:[:digit:]+[:blank:]?"
  ), "hgt\\:", "")))
  %>% mutate(hgt_unit = str_replace(str_extract(
    entry, "hgt\\:[:digit:]+[:alpha:]+[:blank:]?"
  ), "hgt\\:[:digit:]+", ""))
  %>% mutate(hgt_unit = str_replace(hgt_unit, " ", ""))
  %>% mutate(hcl = str_replace(str_extract(
    entry, "hcl\\:#[[:digit:]|[:alpha:]]+[:blank:]?"
  ), "hcl\\:", ""))
  %>% mutate(hcl = str_replace(hcl, " ", ""))
  %>% mutate(ecl = str_replace(str_extract(
    entry, "ecl\\:[:alpha:]+[:blank:]?"
  ), "ecl\\:", ""))
  %>% mutate(ecl = str_replace(ecl, " ", ""))
  %>% mutate(pid = str_replace(str_extract(
    entry, "pid\\:[:digit:]+[:blank:]?"
  ), "pid\\:", ""))
  %>% mutate(pid = str_replace(pid, " ", ""))
  ## Drop unneeded columns
  %>% select(-entry, -keys)
)

## Define list of allowable eye colours
eye_colours <- c("amb", "blu", "brn", "gry",
                 "grn", "hzl", "oth")

## Filter out passports that are not valid (for one reason
## or another)
(passports_valid
  ## Check birth year
  %>% filter(byr >= 1920, byr <= 2002)  
  ## Check issue year
  %>% filter(iyr >= 2010, iyr <= 2020)
  ## Check expiration year
  %>% filter(eyr >= 2020, eyr <= 2030)
  ## Check height
  %>% filter((hgt_unit == "cm" & ## in cms
                hgt_val >= 150 & hgt_val <= 193) |
               (hgt_unit == "in" & ## in ins
                  hgt_val >= 59 & hgt_val <= 76))
  ## Check hair colour (already checked for the presence of
  ## correct characters in the data cleaning step)
  %>% filter(nchar(hcl) == 7)
  ## Check eye colours
  %>% filter(ecl %in% eye_colours)
  ## Check passport number
  %>% filter(nchar(pid) == 9)
  %>% nrow(.)
) -> n_valid

print(glue("There are {n_valid} valid passports."))
