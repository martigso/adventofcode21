#########################
######### Day 7 #########
#########################

rm(list = ls())

library(stringr)

data <- readLines("data/day8.txt")

#### Part 1 ####
p1_data <- sapply(strsplit(data, "\\|"), "[[", 2) %>% str_trim()

p1_data <- strsplit(p1_data, " ")

all_sums <- sapply(1:length(p1_data), function(y){
  
  outputs <- p1_data[[y]] %>% str_split(" ") %>% unlist() %>% 
    str_split("")
  
  sum(sapply(outputs, length) %in% c(2, 4, 3, 7))
  
  
})

# Solution part 1
sum(all_sums)


#### Part 2 ####
display <- letters[1:7]
numbers <- list(num0 = paste(letters[c(1, 2, 3, 5, 6, 7)], collapse = ""),
                num1 = paste(letters[c(3, 6)], collapse = ""),
                num2 = paste(letters[c(1, 3, 4, 5, 7)], collapse = ""),
                num3 = paste(letters[c(1, 3, 4, 6, 7)], collapse = ""),
                num4 = paste(letters[c(2, 3, 4, 6)], collapse = ""),
                num5 = paste(letters[c(1, 2, 4, 6, 7)], collapse = ""),
                num6 = paste(letters[c(1, 2, 4, 5, 6, 7)], collapse = ""),
                num7 = paste(letters[c(1, 3, 6)], collapse = ""),
                num8 = paste(letters[c(1:7)], collapse = ""),
                num9 = paste(letters[c(1, 2, 3, 4, 6, 7)], collapse = ""))

n <- vector("numeric", length(data))

for(k in 1:length(data)){
  tmp <- data[k] %>% str_split("\\|") %>% unlist() %>% str_trim()
  s1 <- tmp[1] %>% str_split(" ") %>% unlist() %>% str_split("")
  
  tmp_2long <- s1[[which(sapply(s1, length) == 2)]]
  tmp_3long <- s1[[which(sapply(s1, length) == 3)]]
  tmp_4long <- s1[[which(sapply(s1, length) == 4)]]
  tmp_5long <- s1[which(sapply(s1, length) == 5)]
  tmp_6long <- s1[which(sapply(s1, length) == 6)]
  tmp_7long <- s1[[which(sapply(s1, length) == 7)]]
  
  config <- vector("character", 7)
  
  # Find the character map for this set
  config[1] <- setdiff(tmp_3long, tmp_2long)
  config[3] <- unlist(sapply(tmp_6long, function(x) setdiff(tmp_2long, x)))
  tmp_6long <- tmp_6long[which(colSums(sapply(tmp_6long, function(x) x %in% tmp_2long)) == 2)]
  
  config[6] <- setdiff(tmp_2long, config[3])
  
  config[4] <- unlist(sapply(tmp_6long, function(x) setdiff(tmp_4long, x)))
  config[5] <- setdiff(tmp_6long[[which(sapply(tmp_6long, function(x) any(x %in% config[4]) == FALSE))]],
                       tmp_6long[[which(sapply(tmp_6long, function(x) any(x %in% config[4])))]])
  
  config[2] <- setdiff(tmp_4long, config[c(3, 4, 6)])
  
  config[7] <- setdiff(tmp_7long, config[1:6])
  
  # g = 5
  # f = 4
  
  
  decode_df <- lapply(s1, function(y){
    # message(y)
    input <- y
    
    clean <- paste(display[config %in% input], collapse = "")
    
    tmp_correct <- sapply(numbers, function(x) x == clean)
    
    correct <- names(tmp_correct[which(tmp_correct == TRUE)]) %>% str_extract("[0-9]+") %>% as.numeric()
    
    decoded <- data.frame(number = correct,
                          string = paste(sort(input), collapse = ""))
    
    return(decoded)
  })
  
  decode_df <- do.call(rbind, decode_df)
  
  s2 <- tmp[2] %>% str_split(" ") %>% unlist() %>% str_split("")
  
  s2 <- lapply(s2, function(x) x %>% sort() %>% paste(collapse = ""))
  
  n[k] <- lapply(s2, function(x) decode_df$number[which(decode_df$string == x)]) %>% 
    unlist() %>% paste(collapse = "") %>% as.numeric()
}

# Solution part2: 
sum(n)