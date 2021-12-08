#########################
######### Day 3 #########
#########################

rm(list = ls())

library(GA) # convert binary to decimal with binary2decimal()
data <- readLines("./data/day3.txt")


### Part 1 ###

# Splitting the binaries into vectors
split <- lapply(1:length(data), function(x){
  
  unlist(strsplit(data[x], "(?<=[0-1])", perl = TRUE))
  
})

# Making holder for collecting binaries
collect <- list()

# Looping over all 12 binaries
for(i in 1:12){
  
  # extracting binaries in i'th position to a vector
  collect[[i]] <- as.character(sapply(split, "[[", i))
}

# making binary result list
binary_result <- lapply(collect, function(x){
  # counting ones and zeroes in each collect[[x]] 
  n_0 <- length(which(x == "0"))
  n_1 <- length(which(x == "1"))
  
  # if there are more zeroes than ones, return 0, else 1
  binary <- ifelse(n_0 > n_1, 0, 1)
  return(binary)
})

# Unlisting for gamma binary
binary_result <- unlist(binary_result)

# Reversing result for epsilon binary
binary_result_reversed <- (binary_result * -1) +1

# Converting gamma and epsilon to decimal
gamma <- binary2decimal(binary_result)
epsilon <- binary2decimal(binary_result_reversed)

# Solution is gamma multiplied by epsilon
gamma * epsilon

### Part 2 ###

# Making a list of data frames for each binary 
# (more comfortable to work with)
o2_co2_base <- lapply(1:length(collect), function(x){
  data.frame(collect[[x]])
})

# Binding to one data frame
o2_co2_base <- do.call(cbind, o2_co2_base)

# Making copy of data frame for o2 values
o2 <- o2_co2_base

# looping over all binaries
for(x in 1:12){
  
  # if there is only one row left, break the loop
  if(nrow(o2) == 1) break
  
  # if there are more ones (or equal) than zeroes in the x'th column
  # only keep rows starting with 1 in the x'th column
  if(sum(o2[, x] == "1") >= sum(o2[, x] == "0")){
    
    o2 <- o2[which(o2[, x] == "1"), ]
    
  } else { # else only keep rows starting with zero
    
    o2 <- o2[which(o2[, x] == "0"), ]
    
  }
  
}

# Making copy of data frame for o2 values
co2 <- o2_co2_base

# looping over all binaries
for(x in 1:12){
  
  # if there is only one row left, break the loop
  if(nrow(co2) == 1) break
  
  # if there are less ones than zeroes in the x'th column
  # only keep rows starting with 1 in the x'th column
  if(sum(co2[, x] == "1") < sum(co2[, x] == "0")){
    
    co2 <- co2[which(co2[, x] == "1"), ]
    
  } else { # else only keep rows starting with zero
    
    co2 <- co2[which(co2[, x] == "0"), ]
    
  }
  
}

# Solution is o2 multiplied with co2 in decimal
binary2decimal(as.numeric(o2)) * binary2decimal(as.numeric(co2))

