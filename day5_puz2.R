## Day 5
##
## Puzzle 2

## Get cleaned data from the first puzzle
source("day5_puz1.R")

## Get min seat ID
min_seat_id <- (seats
  %>% select(seat_id)
  %>% min(.)
)

## Construct full list of seat IDs
seat_id_list <- min_seat_id:max_seat_id

## Get the seats id that is missing from the full list
my_seat_id <- setdiff(seat_id_list, seats %>% pull(seat_id))

## Print missing seat ID (my seat ID)
print(glue("My seat ID is {my_seat_id}."))