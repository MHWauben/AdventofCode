library(magrittr)
library(tidyr)

input <- read.table('2021/Day5/input.txt', header = FALSE, colClasses = "character") %>%
  tidyr::separate(V1, into = c("x1", "y1"), sep = ",") %>%
  tidyr::separate(V3, into = c("x2", "y2"), sep = ",") %>%
  dplyr::mutate_all(as.numeric)
input$V2 <- NULL

# Line-grid goes from 0 to maximum of the input
extent <- max(input)
# Only use horizontal/verticals for part 1:
input_hor_ver <- input[input$x1 == input$x2 | input$y1 == input$y2,]

# Data uses 0-based indexing
line_grid <- matrix(data = 0, nrow = extent+1, ncol = extent+1, dimnames = list(as.character(0:extent), as.character(0:extent)))


for(i in 1:nrow(input_hor_ver)){
  rows <- as.character(seq(from = min(input_hor_ver$x1[i], input_hor_ver$x2[i]), 
              to = max(input_hor_ver$x1[i], input_hor_ver$x2[i])))
  columns <- as.character(seq(from = min(input_hor_ver$y1[i], input_hor_ver$y2[i]), 
                 to = max(input_hor_ver$y1[i], input_hor_ver$y2[i])))
  line_grid[columns, rows] <- line_grid[columns, rows] + 1
}


length(which(line_grid >= 2))

input_diag <- input[input$x1 != input$x2 & input$y1 != input$y2,]
# Create diagonals' points and add them to the line grid:
lg_diag <- line_grid

for(i in 1:nrow(input_diag)){
  rows <- as.character(seq(from = input_diag$x1[i],
                           to = input_diag$x2[i]))
  columns <- as.character(seq(from = input_diag$y1[i], 
                              to = input_diag$y2[i]))
  points <- data.frame(rows = rows, columns = columns)
  for(j in 1:nrow(points)){
    lg_diag[points$columns[j], points$rows[j]] <- lg_diag[points$columns[j], points$rows[j]] + 1
  }
}

length(which(lg_diag >= 2))

