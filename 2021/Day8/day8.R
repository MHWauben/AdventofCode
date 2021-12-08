library(magrittr)

input <- read.delim("2021/Day8/input.txt", header = FALSE, sep = "\n")

# Part 1; only use last 5 numbers
input_split <- gsub(".*\\| ", "", input$V1)
input_split <- strsplit(input_split, split = " ")
input_unique <- lapply(input_split, nchar) %>%
  lapply(., function(x) x %in% c(2, 4, 3, 7))
input_unique %>%
  lapply(., sum) %>%
  unlist(.) %>% sum(.)

# Part 2: find combinations for unique letters
# Calculation aid:

find_codes <- function(num_char){
  codes <- lapply(strsplit(input$V1, split = " "), 
                  function(x) x[nchar(x) == num_char][1])
  codes[sapply(codes, is.null)] <- NA
  return(unlist(codes))
}

# aaaa 
#b    c
#b    c
# dddd
#e    f
#e    f
# gggg

input_2 <- dplyr::mutate(input,
                         ones = find_codes(2),
                         fours = find_codes(4),
                         sevens = find_codes(3),
                         eights = find_codes(7)) %>%
  tidyr::separate(col = V1, into = paste0("V", 1:15), sep = " ") %>%
  dplyr::mutate(a = unlist(purrr::map2(stringr::str_split(sevens, pattern = ""), 
                                   stringr::str_split(ones, pattern = ""), 
                                   setdiff))) %>%
  dplyr::mutate(bd = purrr::map2(stringr::str_split(fours, pattern = ""), 
                                        stringr::str_split(ones, pattern = ""), 
                                        setdiff),
                bd = purrr::map_chr(bd, ~paste(., collapse = ""))) %>%
  dplyr::mutate(aeg = purrr::map2(stringr::str_split(eights, pattern = ""), 
                                 stringr::str_split(fours, pattern = ""), 
                                 setdiff),
                aeg = purrr::map_chr(aeg, ~paste(., collapse = ""))) %>%
  dplyr::mutate(eg = purrr::map2(stringr::str_split(aeg, pattern = ""), 
                                  stringr::str_split(a, pattern = ""), 
                                  setdiff),
                eg = purrr::map_chr(eg, ~paste(., collapse = "")))
