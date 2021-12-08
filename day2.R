#########################
######### Day 2 #########
#########################

rm(list = ls())

data <- readLines("data/day2.txt")

### Part 1 ###

# Reading data

# Extracting direction
direction <- stringr::str_extract(data, "^[a-z]+")

# Extracting units
units <- as.numeric(stringr::str_extract(data, "[0-9]+$"))

# Making a dataframe of directions and units 
df <- data.frame(direction,
                 units)

# Making initial horizontal position
horizontal <- 0

# Making intial depth position
depth <- 0

# Looping over all data points
for(i in 1:nrow(df)){
  
  # If direction is "down"...
  if(df$direction[i] == "down"){
    # increase current depth with units of i
    depth <- depth + df$units[i]
  }
  
  # If direction is "forward"...
  if(df$direction[i] == "forward"){
    # increase current horizontal positions with units of i
    horizontal <- horizontal + df$units[i]
  }
  
  # If direction is "up"...
  if(df$direction[i] == "up"){
    # decrease depth with units of i
    depth <- depth - df$units[i]
  }
  
}

# Solution is horizontal position multiplied by depth
horizontal * depth

### Part 2 ###

# Making new initial positions
horizontal <- 0
depth <- 0
aim <- 0

# Looping over all data points
for(i in 1:nrow(df)){
  
  # If direction is "down"...
  if(df$direction[i] == "down"){
    # increase aim by units of i
    aim <- aim + df$units[i]
  }
  
  # If direction is "up"...
  if(df$direction[i] == "up"){
    # decrease aim by units of i
    aim <- aim - df$units[i]
  }
  
  # If direction is "forward"...
  if(df$direction[i] == "forward"){
    # increase horizontal position with units of i and ...
    horizontal <- horizontal + df$units[i]
    
    # increase depth with aim multiplied by units of i
    depth <- depth + (aim * df$units[i])
  }
  
}

# Solution is horizontal position multiplied by depth
horizontal * depth
