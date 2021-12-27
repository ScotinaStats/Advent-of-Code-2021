# Part 1 -------------------------------------------------------------------------------
## How many lanternfish would there be after 80 days?
library(tidyverse)

input <- data.frame(x = readLines("day6_input.txt"))
input <- as.numeric(unlist(strsplit(input$x[1], ",")))

days <- 0
while(days < 80){
  input <- input - 1
  
  # Check how many new baby lanternfish
  n_kids <- sum(input == -1) 
  
  # Reset count of parent lanternfish to 6 (NOT 7)
  input[input == -1] <- 6 
  
  # Assign count of 8 to baby lanternfish
  input <- c(input, rep(8, n_kids))
  
  days <- days + 1
}

length(input)


# Part 2 -------------------------------------------------------------------------------
## How many lanternfish would there be after 256 days?
library(data.table) # For nifty 'shift' function
input <- data.frame(x = readLines("day6_input.txt"))
input <- as.numeric(unlist(strsplit(input$x[1], ",")))

#input <- c(3,4,3,1,2)
days <- 0
input_table = c(0, 0, table(input), 0, 0, 0, 0)

input_table

while(days < 256){
  input_table = shift(input_table, type = "lead", fill = 0)
  
  # Check how many new baby lanternfish
  n_kids <- input_table[1]
  
  # Reset count of parent lanternfish to 6 (NOT 7)
  input_table[8] <- input_table[8] + input_table[1] # 8th position is counter 6
  input_table[1] <- 0 # Reset first position to 0
  
  # Assign count of 8 to baby lanternfish
  input_table[10] <- n_kids # 10th position is counter 10
  
  days <- days + 1
}
input_table

options(scipen = 999)
sum(input_table)