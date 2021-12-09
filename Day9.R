# Day 9

# Packages ----------------------------------------------------------------
library(data.table)
library(dplyr)
library(raster)

# Data --------------------------------------------------------------------
data9 <-  as.data.table(read.csv("raw_inputs//input_Day9.txt", header=FALSE, colClasses = "character"))
data9 <- as.data.table(do.call('rbind', strsplit(as.character(data9$V1),'')))
data9 <- as.data.frame(lapply(data9, as.integer), stringsAsFactors=FALSE)

# test
### data9 <- data.frame(c("2199943210", "3987894921", "9856789892", "8767896789", "9899965678"))
### names(data9) <- c("V1")
### data9 <- as.data.table(do.call('rbind', strsplit(as.character(data9$V1),'')))
### data9 <- as.data.frame(lapply(data9, as.integer), stringsAsFactors=FALSE)

# Task 1 ------------------------------------------------------------------

# Pad out with 9s
data <- as.data.frame(matrix(9, nrow = nrow(data9) + 2, ncol = ncol(data9) + 2))
for (i in 1:nrow(data9)){
  for (j in 1:ncol(data9)){
    data[i+1, j+1] <-  data9[i, j]
  }
}

# function that finds neighbours for data frames and checks if all higher than centre
is.local.min <- function(data,i,j){
  if (data[i+1,j] > data[i,j] & data[i-1,j] > data[i,j] & data[i,j+1] > data[i,j] & data[i,j-1] > data[i,j]){
    return(1)}
  else {return(0)}
}

# Calculate locations of local minima
num <- 1 # to store local minuma in a vector to use in part 2
local_min <- as.data.frame(matrix(0), nrow = nrow(data), ncol = 4)
for (ii in 2 : (nrow(data) - 1)){
  for (jj in 2 : (ncol(data) - 1)){
    if (is.local.min(data, ii, jj) == 1){
      local_min[num, 1] <- num # numbering to use in part 2
      local_min[num, 2] <- ii # row location to use in part 2
      local_min[num, 3] <- jj # column location to use in part 2
      local_min[num, 4] <- data[ii,jj] # value in data of local minimum
      num <- num + 1
    }
  }
}

# Calculate answer
ans1 <- sum(local_min[,4]+1)


# Task 2 ------------------------------------------------------------------

# Create a matrix to store basins in; Zeroes will be useful later to refer to anything which is not a 9
basin <- as.data.frame(matrix(0, nrow = nrow(data), ncol = ncol(data)))

# Mark the boundaries made from 9s in the basin
for (i in 1:nrow(basin)){
  for (j in 1:ncol(basin)){
    if (data[i, j] == 9){
      basin[i, j] <- -1 # to mark the boundaries
    }
  }
} 

image(as.matrix(basin))

# Mark the local minimum in each basin with the unique number of the basin
for (i in 1:nrow(local_min)){
  basin[local_min[i, 2], local_min[i, 3]] <- local_min[i,1]
}

# Create a function that returns the 4 neighbours
neighbours <- function(data,i,j){
    return(c(data[i+1,j], data[i-1,j], data[i,j+1], data[i,j-1]))
}

# A loop to assign non-zero unique numbers to each group in a basin
repeat{
  for (i in 2:(nrow(basin)-1)){
    for (j in 2:(ncol(basin)-1)){
      if (basin[i, j] == 0 & max(neighbours(basin, i, j)) > 0){
        basin[i, j] <- max(neighbours(basin, i, j))
      }
    }
  }
  if (sum(basin == 0) == 0){
    break
  }
}

# Find sizes of basins
for (i in 1:nrow(local_min)){
  local_min[i,5] <- length(basin[basin == local_min[i,1]])
}

# Find three biggest
biggest_basins <- sort(local_min$V5, decreasing = TRUE)[1:3]

# Find answer
ans2 <- prod(biggest_basins)
