#########################
######### Day 1 #########
#########################

# Read data
data <- readLines("data/day1.txt")


### Part 1 ###

# Converting to numeric
data <- as.numeric(data)

# Making holder variable for whether step is deeper
deeper <- logical(2000)

for(i in 2:length(data)){ # looping over all but first data points
  # if i is bigger than i-1 return TRUE, all else return FALSE
  deeper[i] <- ifelse(data[i] > data[i-1], TRUE, FALSE)
}

# TRUE count is solution
table(deeper)


### Part 2 ###

# Constructing sliding average over 3 and 3 data points
sliding <- lapply(1:(length(data)-2), function(i){
  
  data[i] + data[i + 1] + data[i + 2]
  
})

# Unlisting
sliding <- unlist(sliding)

# Making holder object for logical -- whether sliding average 
# is deeper than the previous
deeper_sliding <- logical(length(sliding))

# Looping over all data points
for(i in 2:length(sliding)){
  # If sliding average is higher than previous, return TRUE
  # else return FALSE
  deeper_sliding[i] <- ifelse(sliding[i] > sliding[i-1], TRUE, FALSE)
}

# True is solution
table(deeper_sliding)
