library(magrittr)
library(tidyr)
input <- read.delim('2021/Day2/input.txt', header = FALSE) %>%
  tidyr::separate(V1, into = c("dir", "value"), sep = " ") %>%
  dplyr::group_by(dir) %>%
  dplyr::summarise(value = sum(as.integer(value))) %>%
  dplyr::mutate(value = ifelse(dir == "up", value * -1, value),
                dir = ifelse(dir %in% c("down", "up"), "vertical", "horizontal")) %>%
  dplyr::group_by(dir) %>%
  dplyr::summarise(value= sum(value))
input$value[1] * input$value[2]

input <- read.delim('2021/Day2/input.txt', header = FALSE) %>%
  tidyr::separate(V1, into = c("dir", "value"), sep = " ") %>%
  dplyr::mutate(value = as.integer(value),
                aim_change = ifelse(dir == "down", value, ifelse(dir == "up", value * -1, 0)),
                current_aim = cumsum(aim_change),
                horiz_change = ifelse(dir == "forward", value, 0),
                depth_change = ifelse(dir == "forward", value * current_aim, 0))
sum(input$horiz_change) * sum(input$depth_change)
