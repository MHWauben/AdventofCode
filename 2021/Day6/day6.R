input <- read.table('2021/Day6/input.txt', header = FALSE) 
input <- as.integer(unlist(strsplit(input[1,1], split = ",")))

for(i in 1:256){
  # Save the spawning fish
  zero_locations <- input == 0
  
  # Reduce all non-zero by 1
  input <- input - 1
  
  # Add new fish
  if(length(which(zero_locations))){
    # Change all 0 to 6
    input[zero_locations] <- 6
    
    # How many new fish?
    input <- append(input, values = 
                      rep(x = 8, 
                          times = length(which(zero_locations)))
                    )
  }
  rm(zero_locations)
  gc()
}

length(input)
