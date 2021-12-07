library(magrittr)
library(dplyr)
input <- as.data.frame(t(read.delim('2021/Day7/input.txt', sep = ",", header = FALSE)))
input$go_to <- median(input$V1)
input$fuel <- abs(input$V1 - input$go_to)
sum(input$fuel)

input$go_to_2 <- round(mean(input$V1), 0) -1
input$dist_2 <- abs(input$V1 - input$go_to_2)
input <- input %>%
  dplyr::rowwise() %>%
  dplyr::mutate(fuel_2 = sum(seq(from = 1, to = dist_2)))
sum(input$fuel_2)
