# Day 8

# Packages ----------------------------------------------------------------
library(data.table)
library(dplyr)

# Data --------------------------------------------------------------------
data8 <-  as.data.table(read.table("raw_inputs//input_Day8.txt", header=FALSE, stringsAsFactors = FALSE))

# Task 1 ------------------------------------------------------------------
ans1 <-  data8[nchar(V12) %in% c(2,3,4,7), .N] + data8[nchar(V13) %in% c(2,3,4,7), .N] + data8[nchar(V14) %in% c(2,3,4,7), .N] + data8[nchar(V15) %in% c(2,3,4,7), .N]


# Task 2 ------------------------------------------------------------------
# Number 1 = {CF} (2) -> can find out CF=?, ABDEG=?, 1s
# Number 7 = {ACF} (3) -> can find out A=? BDEG=?, 7s
# Number 4 = {BCDF} (4) -> can find out BD=?, EG=?, 4s
# Number 8 = {ABCDEFG} (7) -> can find out 8s
# ---
# Number 9 = {ABCDFG} (6) [Only 6 that has BCDF in it] -> can find out AG=?, E=?, 9s
# Number 6 = {ABDEFG} (6) [Only 6 that doesn't have CF in it] -> can find out C=?, F=?, 6s
# Number 0 = {ABCEFG} (6) [Only 6 left that has CF in it] -> can find out D=?, B=? (from 4), 0s
# ---
# Number 3 = {ACDFG} (5) [Only 5 that has CF in it] -> can find out G=?, 3s
# Number 2 = {ACDEG} (5) -> can find out 2s
# Number 5 = {ABDFG} (5) -> can find out 5s

# Get data in list (to allow for different number elements)
dlist <- strsplit(readLines("raw_inputs//input_Day8.txt"), " ")

# test data
### dlist <- strsplit("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf", " ")
### dlist <- strsplit("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe", " ")
### dlist <- strsplit("edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc", " ")
### dlist <- strsplit("fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg", " ")
### dlist <- strsplit(c("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cagedb fcadb cagdeb cdbaf", "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"), " ")
### dlist <- strsplit("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | acedgfb cdfbe gcdfa fbcad", " ")

# Make lists in the list to seperate the letters (to allow for re-ordering)
for (ii in 1:length(dlist)){
  dlist[[ii]] <- strsplit(dlist[[ii]][1:15], "")
}

# Initialise the summation variable
sum <- 0

# Loop through to apply the logic of finding each number
for (i in 1:length(dlist)){
  
  # find the 1s, 7s, 4s and 8s first
  for (j in 1:10){
    if (length(dlist[[i]][[j]]) == 2){
      j1 <- j # find position of the 1
      for (k in 12:15){if (length(union(dlist[[i]][[j]], dlist[[i]][[k]])) - length(intersect(dlist[[i]][[j]], dlist[[i]][[k]])) == 0){dlist[[i]][[k]] <- 1}}}
    else if (length(dlist[[i]][[j]]) == 3){
      j7 <- j # find position of the 7
      for (k in 12:15){if (length(union(dlist[[i]][[j]], dlist[[i]][[k]])) - length(intersect(dlist[[i]][[j]], dlist[[i]][[k]])) == 0){dlist[[i]][[k]] <- 7}}}
    else if (length(dlist[[i]][[j]]) == 4){
      j4 <- j # find position of the 4
      for (k in 12:15){if (length(union(dlist[[i]][[j]], dlist[[i]][[k]])) - length(intersect(dlist[[i]][[j]], dlist[[i]][[k]])) == 0){dlist[[i]][[k]] <- 4}}}
    else if (length(dlist[[i]][[j]]) == 7){
      j8 <- j # find position of the 8
      for (k in 12:15){if (length(union(dlist[[i]][[j]], dlist[[i]][[k]])) - length(intersect(dlist[[i]][[j]], dlist[[i]][[k]])) == 0){dlist[[i]][[k]] <- 8}}}
  }
  
  # calculate sets to use later
  CF <- dlist[[i]][[j1]]
  BCDF <- dlist[[i]][[j4]]
  
  # find the 9s, 6s, 0s and 3s
  for (j in 1:10){
    if (length(dlist[[i]][[j]]) == 6 & all(BCDF %in% dlist[[i]][[j]])){
      j9 <- j # find position of the 9
      for (k in 12:15){if (length(union(dlist[[i]][[j]], dlist[[i]][[k]])) - length(intersect(dlist[[i]][[j]], dlist[[i]][[k]])) == 0){dlist[[i]][[k]] <- 9}}}
    else if (length(dlist[[i]][[j]]) == 6 & !all(CF %in% dlist[[i]][[j]])){
      j6 <- j # find position of the 6
      for (k in 12:15){if (length(union(dlist[[i]][[j]], dlist[[i]][[k]])) - length(intersect(dlist[[i]][[j]], dlist[[i]][[k]])) == 0){dlist[[i]][[k]] <- 6}}}
    else if (length(dlist[[i]][[j]]) == 6){
      j0 <- j # find position of the 0
      for (k in 12:15){if (length(union(dlist[[i]][[j]], dlist[[i]][[k]])) - length(intersect(dlist[[i]][[j]], dlist[[i]][[k]])) == 0){dlist[[i]][[k]] <- 0}}}
     }
  
  # calculate set to use later
  C <- union(setdiff(dlist[[i]][[j6]], dlist[[i]][[j8]]), setdiff(dlist[[i]][[j8]], dlist[[i]][[j6]]))
  
  # find the 2s and 5s
  for (j in 1:10){
    if (length(dlist[[i]][[j]]) == 5 & all(CF %in% dlist[[i]][[j]])){
      j3 <- j # find position of the 3
      for (k in 12:15){if (length(union(dlist[[i]][[j]], dlist[[i]][[k]])) - length(intersect(dlist[[i]][[j]], dlist[[i]][[k]])) == 0){dlist[[i]][[k]] <- 3}}}
    else if (length(dlist[[i]][[j]]) == 5 & !all(C %in% dlist[[i]][[j]])){
      j5 <- j # find position of the 5
      for (k in 12:15){if (length(union(dlist[[i]][[j]], dlist[[i]][[k]])) - length(intersect(dlist[[i]][[j]], dlist[[i]][[k]])) == 0){dlist[[i]][[k]] <- 5}}}
    else if (length(dlist[[i]][[j]]) == 5){
      j2 <- j # find position of the 2
      for (k in 12:15){if (length(union(dlist[[i]][[j]], dlist[[i]][[k]])) - length(intersect(dlist[[i]][[j]], dlist[[i]][[k]])) == 0){dlist[[i]][[k]] <- 2}}}
  }
  sum <- sum + dlist[[i]][[15]] + 10 * dlist[[i]][[14]] + 100 * dlist[[i]][[13]] + 1000 * dlist[[i]][[12]]
}

# Find answer
ans2 <- sum
