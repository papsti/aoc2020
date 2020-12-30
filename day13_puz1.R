## Day 13
##
## Puzzle 1

## Load libraries
library(stringr)
library(glue)

## Read data
data <- readLines(
  "data/day13_puz1_input.txt")

earliest_time <- as.integer(data[1])
routes <- as.integer(
  unlist(str_split(data[2],
                   "[,x]+,?")))

## Find route with earliest possible wait time
mod_time <- earliest_time %% routes
last_departure <- earliest_time - mod_time
next_departure <- last_departure + routes
my_departure <- min(next_departure)

## Which route should I take?
the_route <- routes[which(next_departure == my_departure)]

## What is my wait time?
wait_time <- my_departure - earliest_time

## Print the route ID times the wait time
print(glue({"route ID times wait time: {the_route*wait_time}"}))
