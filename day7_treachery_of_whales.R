# Part 1 -------------------------------------------------------------------------------
## Determine the horizontal position that the crabs can align to using the least fuel possible. 
## How much fuel must they spend to align to that position?
library(tidyverse)

input <- data.frame(x = readLines("day7_input.txt"))
input <- as.numeric(unlist(strsplit(input$x[1], ",")))

min(colSums(as.matrix(dist(input, diag = TRUE, upper = TRUE))))


# Part 2 -------------------------------------------------------------------------------
## How much fuel must they spend to align to that position?
#input <- c(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)

### Sum of 1:n = n(n+1)/2

# Compare each element of 'input' to elements from range of input
input_range <- min(input):max(input)
fuel <- sapply(input_range, function(x) abs(x - input))
min(colSums(fuel*(fuel+1)/2))
