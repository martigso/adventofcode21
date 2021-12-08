#########################
######### Day 7 #########
#########################

data <- scan("./data/day7.txt", sep = ",")
# data <- c(16,1,2,0,4,2,7,1,2,14)
# Part 1

holder <- numeric(range(data)[2])
names(holder) <- paste0("hpos_", 1:range(data)[2])
for(i in 1:range(data)[2]){
  
  holder[i] <- sum(abs(data - i))
  
}

# Solution
holder[which.min(holder)]

plot(holder, 1:length(holder))


# Part 2
holder <- numeric(range(data)[2]+1)
names(holder) <- paste0("hpos_", 0:range(data)[2])

test <- lapply(data, function(x){
  
  tmp <- numeric(length(holder))
  
  for(i in 1:length(holder)){
    tmp[i] <- sum(0:abs(x - i))
  }
  
  return(tmp)
  
})


tmp <- numeric(length(test))

for(i in 1:length(test)){
  
  tmp[i] <- sum(sapply(test, "[[", i))

}

min(tmp)

ggplot(NULL, aes(y = tmp, x = 1:length(tmp), group = 1)) +
  geom_path() +
  geom_segment(aes(x = min(tmp), xend = min(tmp), 
                   y = 0, yend = which.min(tmp)), 
               color = "darkcyan") +
  geom_segment(aes(x = min(tmp), xend = max(tmp), y = which.min(tmp), 
                   yend = which.min(tmp)),
               color = "darkcyan") +
  scale_x_continuous(limits = c(min(tmp) - 5000000, max(tmp))) +
  theme_classic()

