# Day 5

# Packages ----------------------------------------------------------------
# conda install r-data.table
library(data.table)

# Data --------------------------------------------------------------------
data5 <-  as.data.table(read.table("raw_inputs//input_Day5.txt", header=FALSE))
first_coord <- as.data.table(do.call('rbind', strsplit(as.character(data5$V1),',')))
colnames(first_coord) <- c("X1","Y1")
second_coord <- as.data.table(do.call('rbind', strsplit(as.character(data5$V3),',')))
colnames(second_coord) <- c("X2","Y2")
data <- cbind(first_coord, second_coord)


# Task 1 ------------------------------------------------------------------
data_Task1 <- data[X1 == X2 | Y1 == Y2]
x_target <- as.integer(max(max(data_Task1$X1), max(data_Task1$X2)))
y_target <- as.integer(max(max(data_Task1$Y1), max(data_Task1$Y2)))

target <- as.data.frame(matrix(0, nrow = x_target, ncol = y_target))

for (i in 1:nrow(data_Task1)){
  X1 <- as.integer(data_Task1$X1[i])
  X2 <- as.integer(data_Task1$X2[i])
  Y1 <- as.integer(data_Task1$Y1[i])
  Y2 <- as.integer(data_Task1$Y2[i])
  
  if (X1 == X2){
    for (j in Y1:Y2){
      target[X1, j] <- target[X1, j] +1
    }}
  else if (Y1 == Y2){
    for (j in X1:X2){
      target[j, Y1] <- target[j, Y1] +1
    }}
  else {}
}

# calculate answer
counts <- as.data.table(table(unlist(target)))
ans1 <- counts[V1 >  1, sum(N)]


# Task 2 ------------------------------------------------------------------
data_Task2 <- data
x_target <- as.integer(max(max(data_Task2$X1), max(data_Task2$X2)))
y_target <- as.integer(max(max(data_Task2$Y1), max(data_Task2$Y2)))

target <- as.data.frame(matrix(0, nrow = x_target, ncol = y_target))

for (i in 1:nrow(data_Task2)){
  X1 <- as.integer(data_Task2$X1[i])
  X2 <- as.integer(data_Task2$X2[i])
  Y1 <- as.integer(data_Task2$Y1[i])
  Y2 <- as.integer(data_Task2$Y2[i])
  
  if (X1 == X2){
    for (j in Y1:Y2){
      target[X1, j] <- target[X1, j] +1
    }}
  else if (Y1 == Y2){
    for (j in X1:X2){
      target[j, Y1] <- target[j, Y1] +1
    }}
  else if (X1 - X2 == Y1 - Y2){
    for (j in X1:X2){
      target[j, Y1 + (j - X1)] <- target[j, Y1 + (j - X1)] +1
    }}
  else if (X1 - X2 == Y2 - Y1){
    for (j in X1:X2){
      target[j, Y1 + (X1 - j)] <- target[j, Y1 + (X1 - j)] +1
    }}
  else {}
}

# calculate answer
counts <- as.data.table(table(unlist(target)))
ans2 <- counts[V1 >  1, sum(N)]
