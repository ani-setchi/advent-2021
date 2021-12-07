# Day 7

# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
data7 <-  as.data.frame(read.table("raw_inputs//input_Day7.txt", header=FALSE))
data <- as.data.frame(as.integer(t(do.call('rbind', strsplit(as.character(data7$V1),',')))))
colnames(data) <- c("position")


# Task 1 ------------------------------------------------------------------
med <- median(data$position)

ans1 <- 0
for (i in 1:nrow(data)){
  ans1 <- ans1 + abs(data[i,1] - med)
}


# Task 2 ------------------------------------------------------------------
#data <- as.data.frame(c(16,1,2,0,4,2,7,1,2,14))
#colnames(data) <- c("position")

avg <- floor(mean(data$position)) # not sure why floor is needed here rather than round! But I get the correct fuel count with it

ans2 <- 0
for (i in 1:nrow(data)){
    ans2 <- ans2 + (abs(data[i,1] - avg))*(abs(data[i,1] - avg) + 1)/2 # because summing from 1 to N equaly N*(N+1)/2
  }
