#########################
######### Day 7 #########
#########################
rm(list = ls())
library(dplyr)

data <- readLines("data/day9.txt")

lava <- matrix(NA, 100, 100)


for(i in 1:length(data)){
  
  lava[i, ] <- strsplit(data[i], "") %>% 
    unlist() %>% as.numeric()
    
}

lava <- cbind(lava, 99)
lava <- rbind(lava, 99)

least_holder <- numeric(0) 
for(i in 1:100){
  for(j in 1:100){
    message(j)
    current <- lava[i, j]
    
    neighbors <- c(lava[i-1, j],
                   lava[i+1, j],
                   lava[i, j-1],
                   lava[i, j+1])
    
    if(all(neighbors > current) == TRUE){
      least_holder <- append(least_holder, current)
      
      current_updated <- current
      while(current_updated + )
      
    }
    
  }
}
sum(least_holder+1)

least_holder <- append(least_holder, 1)
