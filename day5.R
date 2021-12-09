#########################
######### Day 5 #########
#########################
rm(list = ls())
library(stringr)

data <- readLines("./data/day5.txt")

### Part 1 ###
df <- data.frame(str_extract_all(data, "[0-9]+", simplify = TRUE))
df <- data.frame(apply(df, 2, as.numeric))
names(df) <- c("x1", "y1", "x2", "y2")

vert_hor <- df[which(df$x1 == df$x2 | df$y1 == df$y2), ]

library(ggplot2)


ggplot(vert_hor) +
  geom_segment(aes(x = x1, xend = x2,
                   y = y1, yend = y2))


coord_map <- matrix(0, nrow = 1000, ncol = 1000)

for(i in 1:nrow(vert_hor)){
  coord_map[vert_hor$y1[i]:vert_hor$y2[i], vert_hor$x1[i]:vert_hor$x2[i]] <- 
    coord_map[vert_hor$y1[i]:vert_hor$y2[i], vert_hor$x1[i]:vert_hor$x2[i]] + 1
  
}

# Solution:
length(which(coord_map > 1))

### Part 2 ###

ggplot(df) +
  geom_segment(aes(x = x1, xend = x2,
                   y = y1, yend = y2)) +
  scale_y_continuous(breaks = seq(0, 1000, 10)) +
  scale_x_continuous(breaks = seq(0, 1000, 10))

coord_map <- matrix(0, nrow = 1000, ncol = 1000)

for(i in 1:nrow(df)){
  
  if(df$y1[i] == df$y2[i] | df$x1[i] == df$x2[i]){
    coord_map[df$y1[i]:df$y2[i], df$x1[i]:df$x2[i]] <- 
      coord_map[df$y1[i]:df$y2[i], df$x1[i]:df$x2[i]] + 1
  } else {
    diag(coord_map[df$y1[i]:df$y2[i], df$x1[i]:df$x2[i]]) <- 
      diag(coord_map[df$y1[i]:df$y2[i], df$x1[i]:df$x2[i]] + 1)
  }
  
}

# Solution
length(which(coord_map > 1))

