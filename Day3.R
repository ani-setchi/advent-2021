# Day 3

# Packages ----------------------------------------------------------------
# conda install r-data.table
library(data.table)
library(stringr)

# Data --------------------------------------------------------------------
data3 <- as.data.table(read.table("raw_inputs//input_Day3.txt", header=FALSE, colClasses = "character"))

# Task 1 ------------------------------------------------------------------
# Create split for the characters
data_split <- as.data.frame(stringr::str_split(data3$V1, "", simplify = TRUE))
data_split <- data.frame(lapply(data_split, as.integer), stringsAsFactors=FALSE)
data_split <- data_split - 1 #as.integer refers to position not the value so the -1 makes it equal to the factor value
# Calculate gamma function as vector
gamma_rate <- data.table(matrix(NA_integer_, nrow = 1, ncol = ncol(data_split)))
for (i in 1:ncol(data_split)){
    gamma_rate[1, i] <- sum(data_split[, i])/nrow(data_split) >= 0.5
}
# Calculate epsilon function as vector
epsilon_rate <- 1-gamma_rate
# Calculate numbers
gamma_rate <- paste(gamma_rate, collapse = "")
epsilon_rate <- paste(epsilon_rate, collapse = "")
# Convert to base 10
gamma_rate <- strtoi(gamma_rate, base = 2)
epsilon_rate <- strtoi(epsilon_rate, base = 2)
# Calculate answer
ans1 <- gamma_rate * epsilon_rate


# Task 2 ------------------------------------------------------------------
# Calculate oxygen function as vector
oxygen_rate <- as.data.frame(data_split)
cols <- colnames(oxygen_rate)
for (i in 1:ncol(oxygen_rate)){
  if (sum(oxygen_rate[, i])/nrow(oxygen_rate) >= 0.5) {
    dummy <- 1
  } else {
      dummy <- 0
  } 
  oxygen_rate <- filter(oxygen_rate, get(cols[i]) == dummy)
  if (nrow(oxygen_rate) == 1) break
  }
  
# Calculate CO2 function as vector
CO2_rate <- as.data.frame(data_split)
cols <- colnames(CO2_rate)
for (i in 1:ncol(CO2_rate)){
  if (sum(CO2_rate[, i])/nrow(CO2_rate) < 0.5) {
    dummy <- 0
  } else {
    dummy <- 1
  } 
  CO2_rate <- filter(CO2_rate, get(cols[i]) != dummy)
  if (nrow(CO2_rate) == 1) break
}

# Collapse into single numbers
oxygen_rate <- paste0(oxygen_rate, collapse = "")
CO2_rate <- paste(CO2_rate, collapse = "")
# Convert to base 10
oxygen_rate <- strtoi(oxygen_rate, base = 2)
CO2_rate <- strtoi(CO2_rate, base = 2)
# Calculate answer
ans2 <- oxygen_rate * CO2_rate
