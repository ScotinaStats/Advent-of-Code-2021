# Part 1 -------------------------------------------------------------------------------
## What will your final score be if you choose that board?
library(tidyverse)

input <- data.frame(x = readLines("day4_input.txt"))

num_call <- as.numeric(unlist(strsplit(input$x[1], ",")))

boards = input %>%
  slice(-1) %>% # Remove called numbers
  filter(x != "") %>%
  separate_rows(x, sep = " +", convert = TRUE) %>% 
  filter(!is.na(x)) %>% 
  mutate(board_num = rep(1:(nrow(.)/25), each = 25), 
         row_num = rep(rep(1:5, each = 5), nrow(.)/25),
         col_num = rep(1:5, nrow(.)/5), 
         order_called = match(x, num_call)) %>%
  arrange(board_num, order_called) %>%
  group_by(board_num, row_num) %>%
  mutate(row_call_num = 1:n()) %>%
  group_by(board_num, col_num) %>%
  mutate(col_call_num = 1:n()) %>%
  ungroup()

# Check when each board reaches BINGO
board_win = boards %>%
  group_by(board_num) %>%
  filter(row_call_num == 5 | col_call_num == 5) %>%
  slice(1) %>%
  ungroup() %>%
  slice_min(order_by = order_called) # Board 17 is Winner on Call 22! 

winning_num = board_win$x
winning_call_num = board_win$order_called

# Check Board 17 for score
boards %>%
  filter(board_num == 17) %>%
  mutate(called = (order_called <= winning_call_num)) %>%
  group_by(called) %>%
  summarize(score = sum(x)) %>%
  slice(1) %>%
  pull(score) -> score_unmarked

score_unmarked*winning_num


# Part 2 -------------------------------------------------------------------------------
## Figure out which board will win last. Once it wins, what would its final score be?
board_last = boards %>%
  group_by(board_num) %>%
  filter(row_call_num == 5 | col_call_num == 5) %>%
  slice(1) %>%
  ungroup() %>%
  slice_max(order_by = order_called) # Board 92 is Loser on Call 83! 

winning_num = board_last$x
winning_call_num = board_last$order_called

# Check Board 92 for score
boards %>%
  filter(board_num == 92) %>%
  mutate(called = (order_called <= winning_call_num)) %>%
  group_by(called) %>%
  summarize(score = sum(x)) %>%
  slice(1) %>%
  pull(score) -> score_unmarked

score_unmarked*winning_num
