library(magrittr)

bingo_nums <- as.integer(unlist(strsplit(readLines('2021/Day4/input.txt',n=1), split = ",")))

boards <- read.table('2021/Day4/input.txt', skip = 1)
# Each bingo board is 5 rows
boards$board_num <- sort(rep(seq(from = 1, to = nrow(boards) / 5), 5))
# Split on each 5 rows
boards <- split(boards, boards$board_num)
# Remove helper split column
boards <- lapply(boards, function(x) x[!names(x) == "board_num"])

# For each variable, are any of the bingo numbers in it?
truth_col <- function(column, bingo_num){
  column %in% bingo_num
}

truth_matrix <- function(board, bingo_num){
  # Recreate the whole True / False boards
  truth_matrices <- apply(board, 2, truth_col, bingo_num = bingo_num)
  # Return if they have complete rows / columns
  row_full <- any(rowSums(truth_matrices) == 5)
  col_full <- any(colSums(truth_matrices) == 5)
  bingo <- row_full | col_full
  return(bingo)
}

# Find first bingo
for(i in 5:length(bingo_nums)){
  truth_matrices <- lapply(boards, truth_matrix, bingo_num = bingo_nums[1:i])
  bingo <- any(unlist(truth_matrices))
  if(bingo){
    break
  }
}

# Calculate score from winning board
bingo_board <- names(unlist(truth_matrices)[unlist(truth_matrices)])
winning_board <- boards[[bingo_board]]
winning_nos <- bingo_nums[1:i]
unmarked_nos <- winning_board[!apply(winning_board, 2, truth_col, bingo_num = winning_nos)]
final_score <- sum(unmarked_nos) * bingo_nums[i]


# Find number of bingos at each round
bingos <- c()
for(i in 5:length(bingo_nums)){
  bingo_matrices <- lapply(boards, truth_matrix, bingo_num = bingo_nums[1:i])
  bingos <- append(bingos, sum(unlist(bingo_matrices)))
}
last_round <- min(which(bingos == max(bingos)))
last_number_round <- last_round + 4
last_numbers <- bingo_nums[1:last_number_round]

# Find difference between second-to-last and last round to find last winning table
second_to_last <- unlist(lapply(boards, truth_matrix, bingo_num = bingo_nums[1:(last_number_round-1)]))
last_boards <- unlist(lapply(boards, truth_matrix, bingo_num = bingo_nums[1:(last_number_round)]))
last_board <- boards[!second_to_last & last_boards][[1]]

# Find unmarked numbers and calculate score
last_unmarked_nos <- last_board[!apply(last_board, 2, truth_col, bingo_num = last_numbers)]
last_score <- sum(last_unmarked_nos) * bingo_nums[last_number_round]



