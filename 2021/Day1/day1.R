input <- read.delim('2021/Day1/input.txt', header = FALSE)
input$lagged <- c(NA, input$V1[-2000])
input$diff <- input$V1 - input$lagged
input$dec_in <- ifelse(input$diff < 0, "decrease", "increase")
table(input$dec_in)


library(RcppRoll)
input$roll_sum <- RcppRoll::roll_sum(input$V1, n = 3L, align = "left", fill = NA)
input$roll_lagged <- c(NA, input$roll_sum[-2000])
input$roll_diff <- input$roll_sum - input$roll_lagged
input$roll_in <- ifelse(input$roll_diff < 0, "decrease", ifelse(input$roll_diff == 0, "same", "increase"))
table(input$roll_in)
