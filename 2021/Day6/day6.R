library(magrittr)
library(tidyr)
input <- read.table('2021/Day6/input.txt', header = FALSE) 
input <- as.integer(unlist(strsplit(input[1,1], split = ",")))

input_t <- input
rm(input)
gc()

for(i in 1:256){
  input_wip <- input_t
  zero_locations <- input_t == 0
  rm(input_t)
  gc()
  
  # Reduce all non-zero by 1
  input_wip[input_wip != 0] <- input_wip[input_wip != 0] - 1
  
  # Add new fish
  if(length(which(zero_locations))){
    # Change all 0 to 6
    input_wip[zero_locations] <- 6
    
    # How many new fish?
    num_new <- length(which(zero_locations))
    input_wip <- append(input_wip, values = rep(x = 8, times = num_new))
  }
  rm(zero_locations)
  gc()
  
  # Write result back
  input_t <-  input_wip
  rm(input_wip)
  gc()
}

length(input_t)
