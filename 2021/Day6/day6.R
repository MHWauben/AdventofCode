input <- read.table('2021/Day6/input.txt', header = FALSE) 
input <- as.integer(unlist(strsplit(input[1,1], split = ",")))

# Part 1: brute force
for(i in 1:80){
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


# Part 2: first version didn't scale!
input_2 <- read.table('2021/Day6/input.txt', header = FALSE) 
input_2 <- as.integer(unlist(strsplit(input_2[1,1], split = ",")))
input_2 <- as.data.frame(table(input_2), stringsAsFactors = FALSE)
input_2 <- merge(input_2, data.frame(input_2 = 0:8), all.y = TRUE)
input_2[is.na(input_2)] <- 0
input_2$input_2 <- as.integer(input_2$input_2)

for(j in 1:256){
  input_2 <- input_2 %>%
    dplyr::mutate(input_2 = input_2 - 1,
                  input_2 = ifelse(input_2 == -1, 8, input_2))
  input_2[input_2$input_2 == 6, "Freq"] <- 
    input_2[input_2$input_2 == 8, "Freq"] + 
    input_2[input_2$input_2 == 6, "Freq"]
}

sum(input_2$Freq)
