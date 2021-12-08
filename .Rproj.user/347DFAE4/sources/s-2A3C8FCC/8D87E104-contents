rm(list = ls())

library(stringr)

data <- readLines("data/day8.txt")


# Part 1
p1_data <- sapply(strsplit(data, "\\|"), "[[", 2) %>% str_trim()

p1_data <- strsplit(p1_data, " ")

all_sums <- sapply(1:length(p1_data), function(y){
  
  outputs <- p1_data[[y]] %>% str_split(" ") %>% unlist() %>% 
    str_split("")
  
  sum(sapply(outputs, length) %in% c(2, 4, 3, 7))
  

})

sum(all_sums)


# Part 2
