#########################
######### Day 4 #########
#########################
rm(list = ls())
library(stringr)

data <- readLines("./data/day4.txt")

# Part 1

draw <- data[1]
draw <- unlist(strsplit(draw, ","))


boards <- data[2:length(data)]
boards <- boards[which(boards != "")]

first <- seq(1, length(boards)-4, 5)
last <- seq(5, length(boards), 5)

board_list <- list()
for(i in 1:length(first)){
  board_list[[i]] <- boards[first[i]:last[i]]
}

board_list <- lapply(board_list, function(x){
  gsub("\\s+", " ", x) %>% str_trim()
})

board_list <- lapply(board_list, function(i){
  tmp <- strsplit(i, "\\s",perl = TRUE)
  tmp <- do.call(rbind, tmp)
  return(tmp)
})

new_holder <- board_list

for(i in 1:length(draw)){
  
  new <- lapply(new_holder, function(x){
    
    if(any(x == draw[i])){
      x[which(x == draw[i])] <- TRUE
    }
    
    return(x)
  })
  
  current_sums <- lapply(new, function(y){
    
    row_sum <- apply(y, 1, function(z){
      sum(z == TRUE)
    })
    
    col_sum <- apply(y, 2, function(z){
      sum(z == TRUE)
    })
    
    tmp <- cbind(row_sum, col_sum)
  })
  
  winner <- lapply(current_sums, function(k){
    ifelse(any(k == 5), "winner", "loser") 
  })
  
  if(any(unlist(winner) == "winner")){
    new_holder <- new
    winner <- which(unlist(winner) == "winner")
    new_holder <- new[[winner]]
    break
  }
  new_holder <- new
}

row_winner <- apply(new_holder, 1, function(x){
  sum(as.logical(x))
})

col_winner <- apply(new_holder, 2, function(x){
  sum(as.logical(x))
})

unmarked_sum <- sum(as.numeric(ifelse(new_holder == TRUE, 
                                      NA, 
                                      new_holder)), 
                    na.rm = TRUE)

unmarked_sum * as.numeric(draw[i])


### Part 2 ###

new_holder <- board_list
names(new_holder) <- paste0("board", 1:length(new_holder))

for(i in 1:length(draw)){
  # message(paste0("Iter ", i, ": ", names(new_holder), collapse = ", "))
  
  new <- lapply(new_holder, function(x){
    
    if(any(x == draw[i])){
      x[which(x == draw[i])] <- TRUE
    }
    
    return(x)
  })
  
  current_sums <- lapply(new, function(y){
    
    row_sum <- apply(y, 1, function(z){
      sum(z == TRUE)
    })
    
    col_sum <- apply(y, 2, function(z){
      sum(z == TRUE)
    })
    
    tmp <- cbind(row_sum, col_sum)
  })
  
  winner <- lapply(current_sums, function(k){
    ifelse(any(k == 5), "winner", "loser") 
  })
  
  if(length(new[which(unlist(winner) == "loser")]) < 1){
    new_holder <- new
    break
  }
  
  new_holder <- new[which(unlist(winner) == "loser")]

}

winner_board <- names(new_holder) %>% str_remove("board") %>% as.numeric()
new_holder <- unlist(new_holder)
unmarked_sum <- sum(as.numeric(ifelse(new_holder == TRUE, NA, new_holder)), na.rm = TRUE)

unmarked_sum * as.numeric(draw[i])
