# Day 11

# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
#data11 <-  as.data.frame(read.csv("raw_inputs//input_Day11_test.txt", header=FALSE, colClasses = "character"))
data11 <-  as.data.frame(read.csv("raw_inputs//input_Day11.txt", header=FALSE, colClasses = "character"))
data11 <- strsplit(as.character(data11$V1), split = "")
data11 <- lapply(data11, as.integer, stringsAsFactors=FALSE)
data11 <- unlist(data11)
data11 <- as.data.frame(matrix(data11, nrow = 10, byrow = TRUE))

# pad out the boundaries
data_pad <- as.data.frame(matrix(-10000, nrow = nrow(data11) + 2, ncol = ncol(data11) + 2))
for (i in 1:nrow(data11)){
  for (j in 1:ncol(data11)){
    data_pad[i+1, j+1] <-  data11[i, j]}}

# Task 1 ------------------------------------------------------------------
data <- data_pad
# function that finds neighbours and adds 1 to all of them if middle is 10
flash.neighbours <- function(data,i,j){
  if (data[i,j] == 10){
    if (data[i-1,j-1] != 10){data[i-1,j-1] <- data[i-1,j-1] + 1}
    if (data[i-1,j] != 10){data[i-1,j] <- data[i-1,j] + 1}
    if (data[i-1,j+1] != 10){data[i-1,j+1] <- data[i-1,j+1] + 1}
    if (data[i,j-1] != 10){data[i,j-1] <- data[i,j-1] + 1}
    data[i,j] <- data[i,j] + 1
    if (data[i,j+1] != 10){data[i,j+1] <- data[i,j+1] + 1}
    if (data[i+1,j-1] != 10){data[i+1,j-1] <- data[i+1,j-1] + 1}
    if (data[i+1,j] != 10){data[i+1,j] <- data[i+1,j] + 1}
    if (data[i+1,j+1] != 10){data[i+1,j+1] <- data[i+1,j+1] + 1}
    return(data)}
  else {return(data)}
}

# introduce counter for number of flashes
sum <- 0

# resolve flashes and gains of 1 at each step
for (n in 1 : 100){
  # the energy level of each octopus increases by 1
  data <- data + 1
  # resolve flashes
  repeat{
    for (i in 2:(nrow(data)-1)){
      for (j in 2:(ncol(data)-1)){
        data <- flash.neighbours(data,i,j)}}
    if (length(data[data == 10]) == 0){
      break}}
  # reset level to 0 after a flash
  data[data > 10] <- 0
  sum <- sum + length(data[data == 0])}

# answer
ans1 <-  sum


# Task 2 ------------------------------------------------------------------

# start with the data agin
data <- data_pad

# resolve flashes and gains of 1 at each step
for (n in 1 : 1000){
  # the energy level of each octopus increases by 1
  data <- data + 1
  # resolve flashes
  repeat{
    for (i in 2:(nrow(data)-1)){
      for (j in 2:(ncol(data)-1)){
        data <- flash.neighbours(data,i,j)}}
    if (length(data[data == 10]) == 0){
      break}}
  # reset level to 0 after a flash
  data[data > 10] <- 0
  if (length(data[data == 0]) == 100){
    break}}

# answer
ans2 <-  n
