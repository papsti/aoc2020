## Day 7
##
## Puzzle 2

print("this is a mess and nothing works lol")

## Load libraries
# library(readr)
# library(tidyr)
# library(stringr)
# library(dplyr)
# library(Matrix)
# library(glue)
# 
# ## Read data
# bags <- read_delim("data/day7_puz1_input.txt",
#                    delim = "\n",
#                    col_names = "line")
# 
# ## Clean data
# (bags 
#   %>% separate(line, 
#                into = c("bag", "contains"),
#                sep = " bags contain ")
#   ## strip " bags."
#   %>% mutate(contains = 
#                str_replace_all(contains,
#                                "[:blank:]?bag[s]?\\.",
#                                ""))
#   ## replace " bags," with ";" as a separator
#   %>% mutate(contains = 
#                str_replace_all(contains,
#                                "[:blank:]?bag[s]?\\,",
#                                ";"))
#   ## convert contains character column into list column by
#   ## spliting contains string into list of strings
#   ## (each list entry is a different bag that's contained)
#   %>% mutate(contains =
#                str_split(contains, ";"))
#   ## unnest list column into long format
#   %>% unnest(contains)
#   ## make column with quantity of bags
#   %>% mutate(n_contains = str_extract(contains,
#                                       "[:digit:]+"))
#   ## strip value of bags and whitespace from start or end of
#   ## entry in contains column
#   %>% mutate(contains = 
#                str_replace_all(contains,
#                                "^[:blank:]?[:digit:]+[:blank:]?|[:blank:]{1}$",
#                                ""))
# ) -> bags
# 
# in_shiny_gold <- tibble(
#   bag = c(),
#   contains = c(),
#   n_contains = c()
# )
# current_bags <- c("shiny gold")
# 
# ## i am... not proud of this
# for (i in 1:nrow(bags %>% distinct(bag))){
#   (bags
#     %>% filter(bag %in% current_bags)
#   ) -> in_current
# 
#   current_bags <- select(in_current, contains)
#   new_rows <- filter(bags, bag %in% current_bags)
#   
#   in_shiny_gold <- bind_rows(in_shiny_gold,
#                              new_rows)
# }
# 
# #########
# 
# bags %>% group_by(bag) %>% nest() -> nested_bags
# 
# (nested_bags
#   %>% filter(bag == "shiny gold")
#   %>% pull(data))
# 
# # the_bag <- "shiny gold"
# # (bags
# #   %>% filter(bag == the_bag)
# #   %>% select(contains, n_contains)
# #   %>% rename(bag = contains)
# # ) -> current_bags
# 
# inner_join(current_bags, bags)
# 
# ## for sure there is a more efficient way to do the
# ## following with a map function from purrr but i don't have
# ## the time to figure it out right now
# # for (row in 1:nrow(current_bags)){
# #   the_bag <- current_bags$contains[row]
# #   the_n <- as.numeric(current_bags$n_contains[row])
# #   
# #   filter(bags, bag == the_bag)
# # }
# 
# (bags
#   %>% filter(bag %in% current_bags$contains)
# ) -> current_contains
# 
# bags %>% filter(bag == "wavy gold")
