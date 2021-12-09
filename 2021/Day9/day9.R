library(magrittr)
library(tidyr)
library(RcppRoll)

input <- read.delim('2021/Day9/example.txt', header = FALSE, sep = "", colClasses = "character") 
length_input <- nchar(input$V1[1])
input_prep <- input %>%
  tidyr::separate(V1, into = paste0("V", 1:length_input), sep = "(?<=.)") %>%
  dplyr::mutate_all(as.numeric)


# Part 1: find local minima
local_min <- function(column){
  minima <- data.frame(column = RcppRoll::roll_min(column, n = 3, 
                                                   align = 'center', 
                                                   fill = NA)) %>%
    tidyr::fill(., column, .direction = "updown") %>%
    .[["column"]]
  return(column == minima)
}

col_mins <- apply(input_prep, 2, local_min)
row_mins <- t(apply(input_prep, 1, local_min))

local_mins <- input_prep[col_mins & row_mins]
# Sometimes there's a whole field of 9s; remove these
local_mins <- local_mins[local_mins != 9]

risk_level <- sum(local_mins + 1)

# Part 2: find basins (sequences of heights going down)
basins <- function(column){
  # column <- as.vector(t(input_prep[3,]))
  minima <- data.frame(column = RcppRoll::roll_min(column, n = 2, 
                                                   align = 'center', 
                                                   fill = NA)) %>%
    tidyr::fill(., column, .direction = "updown") %>%
    .[["column"]]
  
  basins <- data.frame(left_basin = column == dplyr::lag(column, n = 1) + 1,
                       right_basin = column == dplyr::lead(column, n = 1) + 1,
                       basin_centre = column == minima, 
                       is_nine = column == 9) %>%
    tidyr::fill(left_basin, .direction = "up") %>%
    tidyr::fill(right_basin, .direction = "down")
  
  basins_tf <- (basins$left_basin | basins$right_basin | basins$basin_centre) & !basins$is_nine
  start_num <- 1
  basins_numeric <- c()
  for(i in 1:length(basins_tf)){
    if(basins_tf[i] & i == 1L){
      basins_numeric[i] <- start_num
    } else if(basins_tf[i] && basins_tf[i-1]){
      basins_numeric[i] <- start_num
    } else if(basins_tf[i] && !basins_tf[i-1]){
      if(any(basins_numeric == 1)){
        start_num <- start_num + 1
      }
      basins_numeric[i] <- start_num
    } else {
      basins_numeric[i] <- 0
    }
  }
  return(basins_numeric)
}

(col_basins <- apply(input_prep, 2, basins))
(row_basins <- t(apply(input_prep, 1, basins)))

col_basins + row_basins
