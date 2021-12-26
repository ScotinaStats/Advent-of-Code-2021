# Part 1 -------------------------------------------------------------------------------
## What do you get if you multiply your final horizontal position by your final depth?
library(tidyverse)

input <- data.frame(x = readLines("day2_input.txt"))

input %>%
  separate(x, into = c("direction", "distance"), sep = " ") %>%
  mutate(distance = as.numeric(distance), 
         distance = ifelse(direction == "up", -distance, distance), 
         direction = ifelse(direction %in% c("up", "down"), "depth", "horizontal")) %>%
  group_by(direction) %>%
  summarize(final_position = sum(distance)) %>%
  pull(final_position) %>%
  prod()
  
# Part 2 -------------------------------------------------------------------------------
## What do you get if you multiply your final horizontal position by your final depth?

input %>%
  separate(x, into = c("direction", "distance"), sep = " ") %>%
  mutate(distance = as.numeric(distance), 
         distance = ifelse(direction == "up", -distance, distance),
         aim_count = ifelse(direction == "forward", 0, distance), 
         aim = cumsum(aim_count), 
         horizontal = cumsum(ifelse(direction == "forward", distance, 0)), 
         depth = cumsum(ifelse(direction == "forward", distance*aim, 0))) %>%
  slice_tail() %>%
  summarize(final = horizontal*depth)


