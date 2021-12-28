# Part 1 -------------------------------------------------------------------------------
## In the output values, how many times do digits 1, 4, 7, or 8 appear?
library(tidyverse)

input <- data.frame(x = readLines("day8_input.txt"))

input <- input %>%
  separate(x, into = c("entry", "output"), sep = "\\| +")

### Look for digits in the output column that contain 2, 3, 4, or 7 letters
input %>%
  separate(output, into = c("d1", "d2", "d3", "d4"), sep = " ") %>% 
  mutate(across(d1:d4, str_length)) %>% 
  summarize(
    across(d1:d4, function(x) sum(x %in% c(2, 3, 4, 7)))
    ) %>%
  sum()


# Part 2 -------------------------------------------------------------------------------
## In the output values, how many times do digits 1, 4, 7, or 8 appear?

# 0:    *1:      2:      3:     *4:
#  aaaa    ....    aaaa    aaaa    ....
# b    c  .    c  .    c  .    c  b    c
# b    c  .    c  .    c  .    c  b    c
#  ....    ....    dddd    dddd    dddd
# e    f  .    f  e    .  .    f  .    f
# e    f  .    f  e    .  .    f  .    f
#  gggg    ....    gggg    gggg    ....

# 5:      6:    *7:     *8:      9:
#  aaaa    aaaa    aaaa    aaaa    aaaa
# b    .  b    .  .    c  b    c  b    c
# b    .  b    .  .    c  b    c  b    c
#  dddd    dddd    ....    dddd    dddd
# .    f  e    f  .    f  e    f  .    f
# .    f  e    f  .    f  e    f  .    f
#  gggg    gggg    ....    gggg    gggg

digits = input %>%
  select(entry) %>%
  separate_rows(entry, sep = " ") %>% 
  filter(str_length(entry) > 0) %>%
  mutate(entry_num = rep(1:nrow(input), each = 10))

out = input %>%
  select(output) %>%
  separate(output, into = c("d1", "d2", "d3", "d4"), sep = " ") %>%
  pivot_longer(cols = d1:d4, 
               names_to = "d", 
               values_to = "output") %>%
  mutate(entry_num = rep(1:nrow(input), each = 4))

val_function = function(group_num){
  
  test = digits %>%
    filter(entry_num == group_num)
  
  test_out = out %>%
    filter(entry_num == group_num)
  
  # Simple cases first
  # Extract patterns from simple cases (4 and 7 most informative)
  test_1 = str_split(test$entry[str_length(test$entry) == 2], pattern = "")[[1]]
  test_4 = str_split(test$entry[str_length(test$entry) == 4], pattern = "")[[1]]
  test_7 = str_split(test$entry[str_length(test$entry) == 3], pattern = "")[[1]]
  test_8 = str_split(test$entry[str_length(test$entry) == 7], pattern = "")[[1]]
  
  test = test %>%
    mutate(entry = map_chr(entry, 
                           ~paste(sort(strsplit(., "")[[1]]), collapse = ""))) %>%
    mutate(number = case_when(
      str_length(entry) == 2 ~ 1, 
      str_length(entry) == 4 ~ 4, 
      str_length(entry) == 3 ~ 7, 
      str_length(entry) == 7 ~ 8, 
      str_length(entry) == 6 & 
        map_dbl(entry, ~sum(str_detect(., test_4))) == 4 ~ 9, 
      str_length(entry) == 6 & 
        map_dbl(entry, ~sum(str_detect(., test_1))) == 2 & 
        map_dbl(entry, ~sum(str_detect(., test_4))) == 3 ~ 0, 
      str_length(entry) == 5 &
        map_dbl(entry, ~sum(str_detect(., test_1))) == 2 ~ 3,
      str_length(entry) == 5 & 
        map_dbl(entry, ~sum(str_detect(., test_4))) == 3 & 
        map_dbl(entry, ~sum(str_detect(., test_1))) == 1 ~ 5, 
      str_length(entry) == 5 & 
        map_dbl(entry, ~sum(str_detect(., test_4))) == 2 ~ 2, 
      TRUE ~ 6
      #sum(str_detect(entry, test_4)) == 4 ~ 9
    )) 
  
  test_out %>%
    mutate(output = map_chr(output, 
                            ~paste(sort(strsplit(., "")[[1]]), collapse = ""))) %>%
    left_join(test, by = c("output" = "entry")) %>%
    summarize(value = as.numeric(paste(number, collapse = "")))
}

1:nrow(input) %>% 
  map(val_function) %>% 
  unlist() %>% 
  sum()

##################################################
##################################################
##################################################

## PRACTICE WITH ONE ROW!  
test = digits %>%
  slice(1:10)

test_out = out %>%
  slice(1:4)

# Simple cases first
# Extract patterns from simple cases (4 and 7 most informative)
test_1 = str_split(test$entry[str_length(test$entry) == 2], pattern = "")[[1]]
test_4 = str_split(test$entry[str_length(test$entry) == 4], pattern = "")[[1]]
test_7 = str_split(test$entry[str_length(test$entry) == 3], pattern = "")[[1]]
test_8 = str_split(test$entry[str_length(test$entry) == 7], pattern = "")[[1]]

test = test %>%
  mutate(entry = map_chr(entry, 
                         ~paste(sort(strsplit(., "")[[1]]), collapse = ""))) %>%
  mutate(number = case_when(
    str_length(entry) == 2 ~ 1, 
    str_length(entry) == 4 ~ 4, 
    str_length(entry) == 3 ~ 7, 
    str_length(entry) == 7 ~ 8, 
    str_length(entry) == 6 & 
      map_dbl(entry, ~sum(str_detect(., test_4))) == 4 ~ 9, 
    str_length(entry) == 6 & 
      map_dbl(entry, ~sum(str_detect(., test_1))) == 2 & 
      map_dbl(entry, ~sum(str_detect(., test_4))) == 3 ~ 0, 
    str_length(entry) == 5 &
      map_dbl(entry, ~sum(str_detect(., test_1))) == 2 ~ 3,
    str_length(entry) == 5 & 
      map_dbl(entry, ~sum(str_detect(., test_4))) == 3 & 
      map_dbl(entry, ~sum(str_detect(., test_1))) == 1 ~ 5, 
    str_length(entry) == 5 & 
      map_dbl(entry, ~sum(str_detect(., test_4))) == 2 ~ 2, 
    TRUE ~ 6
    #sum(str_detect(entry, test_4)) == 4 ~ 9
  )) 

test_out %>%
  mutate(output = map_chr(output, 
                         ~paste(sort(strsplit(., "")[[1]]), collapse = ""))) %>%
  left_join(test, by = c("output" = "entry")) %>%
  summarize(value = as.numeric(paste(number, collapse = "")))








