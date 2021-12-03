library(magrittr)
library(tidyr)
library(dplyr)
input <- read.table('2021/Day3/input.txt', header = FALSE, colClasses = c("character"))%>%
  tidyr::separate(V1, into = as.character(1:12), sep = "(?<=[01])")
most_common <- function(x){
  most <- head(sort(table(x), decreasing = TRUE), 2)
  if(most[1] == most[2]){
    value <- 1
  } else {
    value <- names(most[1])
  }
  return(value)
}
least_common <- function(x){
  least <- head(sort(table(x), decreasing = FALSE), 2)
  if(least[1] == least[2]){
    value <- 0
  } else {
    value <- names(least[1])
  }
  return(value)
}
gamma <- paste0(apply(input, 2, most_common), collapse = "")
epsilon <- paste0(apply(input, 2, least_common), collapse = "")

strtoi(gamma, base = 2) * strtoi(epsilon, base = 2)


ox_table <- input
for(i in 1:(ncol(input)-1)){
  if(nrow(ox_table) == 1){
    break
  }
  ox_table <- ox_table[ox_table[[i]] == most_common(ox_table[[i]]),]
}
if(nrow(ox_table) > 1){
  ox_table <- ox_table[ox_table[[ncol(input)]] == 1,]
}
oxygen <- paste0(ox_table[1,], collapse = "")

co2_table <- input
for(i in 1:(ncol(input)-1)){
  if(nrow(co2_table) == 1){
    break
  }
  co2_table <- co2_table[co2_table[[i]] == least_common(co2_table[[i]]),]
}
if(nrow(co2_table) > 1){
  co2_table <- co2_table[co2_table[[ncol(input)]] == 1,]
}
co2 <- paste0(co2_table[1,], collapse = "")

strtoi(oxygen, base = 2) * strtoi(co2, base = 2)
