# Part 1 -------------------------------------------------------------------------------
## Consider only horizontal and vertical lines. At how many points do at least two lines overlap?
library(tidyverse)

input <- data.frame(x = readLines("day5_input.txt"))
input %>%
  mutate(x = str_remove(string = x, pattern = "->")) %>%
  separate(x, into = c("x1", "y1", "x2", "y2"), sep = ",|  ") %>%
  filter((x1 == x2) | (y1 == y2)) %>%
  mutate(line_num = row_number()) %>%
  group_by(line_num) %>%
  expand(x_coords = x1:x2, y_coords = y1:y2) %>%
  ungroup() %>%
  count(x_coords, y_coords) %>%
  summarize(sum(n >= 2))


  #summarize(x_coords = list(x1:x2), 
   #         y_coords = list(y1:y2)) 
  
table(input$x_coords, input$y_coords)

coords = data.frame(x_coords = unlist(input$x_coords), 
                    y_coords = unlist(input$y_coords))
sum(table(unlist(input$x_coords)) >= 2)
sum(table(unlist(input$y_coords)) >= 2)


# Part 2 -------------------------------------------------------------------------------
## Consider all of the lines. At how many points do at least two lines overlap?
input = input %>%
  mutate(x = str_remove(string = x, pattern = "->")) %>%
  separate(x, into = c("x1", "y1", "x2", "y2"), sep = ",|  ") %>%
  mutate(line_num = row_number()) 

### Gather straight line coordinates
input_straight_pts = input %>%
  filter((x1 == x2) | (y1 == y2)) %>%
  group_by(line_num) %>%
  expand(x_coords = x1:x2, y_coords = y1:y2) %>%
  ungroup() 
  count(x_coords, y_coords) %>%
  summarize(sum(n >= 2))

### Gather diagonal line coordinates
### Use unnest to flatten list columns into regular
### Preserves order of x1:x2 if x1 > x2
input_diag_pts = input %>%
  mutate(across(everything(), as.numeric)) %>%
  filter((abs(x2 - x1) == abs(y2 - y1))) %>%
  group_by(line_num) %>%
  summarize(x_coords = list(x1:x2), 
            y_coords = list(y1:y2)) %>% 
  unnest(c(x_coords, y_coords)) 
  
### Combine coordinates
### (Some straight line points might share coords with diagonal line points)
bind_rows(input_straight_pts, input_diag_pts) %>%
  count(x_coords, y_coords) %>% 
  summarize(sum(n >= 2))


