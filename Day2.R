# Day 2

# Packages ----------------------------------------------------------------
# conda install r-data.table
library(data.table)

# Data --------------------------------------------------------------------
data2 <- as.data.table(read.table("raw_inputs//input_Day2.txt", header=FALSE))

# Task 1 ------------------------------------------------------------------
# Calculate difference
aggr <- data2[ , sum(V2), V1]
# Calculate answer
ans1 <- as.integer((aggr[V1 == 'forward', ][1,2]) * (aggr[V1 == 'down', ][1,2] - aggr[V1 == 'up', ][1,2]))

# Task 2 ------------------------------------------------------------------
# Calculate forward
data2[ V1 == 'forward' , forward_flag := 1][ V1 == 'up' , forward_flag := 0][ V1 == 'down' , forward_flag := 0]
data2[, forward_aggr := cumsum(forward_flag * V2)]
# Calculate aim
data2[ V1 == 'forward' , aim_flag := 0][ V1 == 'up' , aim_flag := -1][ V1 == 'down' , aim_flag := 1]
data2[, aim := cumsum(aim_flag * V2)]
# Calculate depth
data2[, dep_aggr := cumsum(aim * forward_flag * V2)]
# Calculate answer
ans2 <- tail(data2, 1L)[1,4] * tail(data2, 1L)[1,7]
