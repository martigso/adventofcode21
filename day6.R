#########################
######### Day 5 #########
#########################
rm(list = ls())

start_p1 <- Sys.time()

data <- readLines("./data/day6.txt")

data <- strsplit(data, ",")
data[[1]] <- as.numeric(data[[1]])
data[2:81] <- numeric(1)

for(x in 2:length(data)){
  
  # if(x == 1) next # return(data[[x]])
  
  data[[x]] <- data[[x-1]] - 1
  
  data[[x]] <- c(data[[x]], rep(8, length(which(data[[x]] == -1))))
  
  data[[x]] <- ifelse(data[[x]] == -1, 6, data[[x]])
  
}

# Solution
length(data[[81]])

end_p1 <- Sys.time()



### Part 2 ###
start_p2 <- Sys.time()

suppressMessages(library(dplyr))

counts <- data.frame(count = c(0, tabulate(data[[1]], nbins = 8)))
counts$name <- 0:8
counts <- counts[, c("name", "count")]


for(i in 1:256){
  
  if(i > 1){
    counts$count <- counts$new_count
  } else {
    counts$new_count <- NA
  }
  
  counts <- counts %>% 
    mutate(new_count = lead(count, default = 0))
  
  zeros <- counts$count[which(counts$name == 0)]
  counts$new_count[which(counts$name == 6)] <- counts$new_count[which(counts$name == 6)] + zeros
  counts$new_count[which(counts$name == 8)] <- counts$new_count[which(counts$name == 8)] + zeros

}

options(digits = 22)

# Solution
sum(counts$new_count)

end_p2 <- Sys.time()

difftime(end_p1, start_p1)
difftime(end_p2, start_p2)
