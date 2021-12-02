# Day 1

# Packages ----------------------------------------------------------------
# conda install r-data.table
library(data.table)

# Data --------------------------------------------------------------------
data1 <- as.data.table(read.table("raw_inputs//input_Day1.txt", header=FALSE))

# Task 1 ------------------------------------------------------------------
# Calculate difference
data1[ , stat := V1 - shift(V1)]
# Count positives
ans1 <- data1[stat > 0, .N]

# Task 2 ------------------------------------------------------------------
# Do groupings for second exercise
data1[ , stat2 := V1 - shift(V1, n=3)]
# Count positives
ans2 <- data1[stat2 > 0, .N]
