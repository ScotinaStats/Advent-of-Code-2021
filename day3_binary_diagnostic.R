# Part 1 -------------------------------------------------------------------------------
## What is the power consumption of the submarine?
library(tidyverse)

input <- data.frame(x = readLines("day3_input.txt"))

input_list = strsplit(input$x, split = "")

input = do.call(rbind.data.frame, input_list)
for (i in 1:ncol(input)){
  names(input)[i] <- paste0("b", i)
}

bin_vec = 2^(11:0)
input %>%
  mutate(across(everything(), ~as.numeric(.))) %>%
  summarize_all(mean) %>%
  t() %>%
  as.data.frame() %>%
  mutate(gamma = ifelse(V1 >= 0.5, 1, 0), 
         epsilon = ifelse(V1 < 0.5, 1, 0), 
         bin_vec = bin_vec) %>%
  summarize(gamma_rate = sum(gamma*bin_vec), 
            epsilon_rate = sum(epsilon*bin_vec)) %>%
  prod()

# Part 2 -------------------------------------------------------------------------------
## What is the power consumption of the submarine?

### oxygen generator rating
input = input %>%
  mutate(across(everything(), ~as.numeric(.)))

input_oxy = input
for(i in 1:ncol(input_oxy)){
  if(mean(input_oxy[, i]) >= 0.5){
    input_oxy = input_oxy[input_oxy[, i] == 1, ]
  } else{
    input_oxy = input_oxy[input_oxy[, i] == 0, ]
  }
  num_row = nrow(input_oxy)
  
  if(num_row == 1){
    break
  }
}

oxy_rating = sum(input_oxy * bin_vec) # 1935

### CO2 scrubber rating
input_co2 = input
for(i in 1:ncol(input_co2)){
  if(mean(input_co2[, i]) >= 0.5){
    input_co2 = input_co2[input_co2[, i] == 0, ]
  } else{
    input_co2 = input_co2[input_co2[, i] == 1, ]
  }
  num_row = nrow(input_co2)
  
  if(num_row == 1){
    break
  }
}

co2_rating = sum(input_co2 * bin_vec) # 3145

oxy_rating * co2_rating ## 6085575
