# Part 1 -------------------------------------------------------------------------------
## Count the number of times a depth measurement increases from the previous measurement

measurements <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

sum(diff(measurements) > 0)

input = as.numeric(readLines("day1_input.txt"))
sum(diff(input) > 0)

# Part 2 -------------------------------------------------------------------------------
## Consider sums of a three-measurement sliding window. How many sums are larger than the previous sum?
library(RcppRoll)

sums <- roll_sum(x = input, n = 3)
sum(diff(sums) > 0)
