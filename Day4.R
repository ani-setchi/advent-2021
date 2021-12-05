# Day 4

# Packages ----------------------------------------------------------------
# conda install r-data.table
library(data.table)
library(stringr)

# Data --------------------------------------------------------------------
data4order <- read.csv("raw_inputs//input_Day4A.txt", header=FALSE)
data4tables_rows <- as.data.table(read.table("raw_inputs//input_Day4B.txt", header=FALSE))
data4tables_cols <- as.data.table(read.table("raw_inputs//input_Day4B.txt", header=FALSE))

# Task 1 ------------------------------------------------------------------
# Transform the data for rows ---
Rows <- data4tables_rows[, table := ceiling(.I/5)][, orient := 'Row']

# Transform the data for columns ---
Cols <- as.data.frame(matrix(NA, nrow = nrow(data4tables_cols), ncol = ncol(data4tables_cols)))
dummy <- as.data.frame(t(data4tables_cols))
for (i in 1:nrow(data4tables_cols)){
  for (j in 1:ncol(data4tables_cols)){
    Cols[5*(ceiling(i/5)-1) + j, i%%5+1] <- dummy[j, i]
  }
}
Cols <- as.data.table(Cols)
Cols <- Cols[, table := ceiling(.I/5)][, orient := 'Column']

# Combine the data for rows and columns ---
data <- rbind(Rows, Cols)

data_Task1 <- data
# Check if numbers are in rows and columns
for (i in 1:ncol(data4order)){
  data_Task1 <- data_Task1[V1 == data4order[1,i], V1 := 0]
  data_Task1 <- data_Task1[V2 == data4order[1,i], V2 := 0]
  data_Task1 <- data_Task1[V3 == data4order[1,i], V3 := 0]
  data_Task1 <- data_Task1[V4 == data4order[1,i], V4 := 0]
  data_Task1 <- data_Task1[V5 == data4order[1,i], V5 := 0]
  data_Task1 <- data_Task1[, check := V1 + V2 + V3 + V4 +V5]
  if (prod(data_Task1$check) == 0) break
}

# Find last calling number
last_call <- data4order[1,i]

# Find other numbers in bingo winner table
winner_no <- data_Task1[check == 0, table]
winner_table <- data_Task1[table == winner_no][orient == "Row"]
other_nos <- sum(winner_table$V1) + sum(winner_table$V2) +sum(winner_table$V3) + sum(winner_table$V4) + sum(winner_table$V5)

# Answer
ans1 <- last_call * other_nos


# Task 2 ------------------------------------------------------------------
data_Task2 <- data
number_of_tables_left <- max(data_Task2$table)
# Check if numbers are in rows and columns
for (i in 1:ncol(data4order)){
  data_Task2 <- data_Task2[V1 == data4order[1,i], V1 := 0]
  data_Task2 <- data_Task2[V2 == data4order[1,i], V2 := 0]
  data_Task2 <- data_Task2[V3 == data4order[1,i], V3 := 0]
  data_Task2 <- data_Task2[V4 == data4order[1,i], V4 := 0]
  data_Task2 <- data_Task2[V5 == data4order[1,i], V5 := 0]
  data_Task2 <- data_Task2[, check := V1 + V2 + V3 + V4 +V5]
  
  for (j in 1:max(data_Task2$table)){
    table_to_check <- data_Task2[table == j]
    if (prod(table_to_check$check) == 0) {
      data_Task2 <- data_Task2[table != j]
      number_of_tables_left <- number_of_tables_left - 1
    }
  }
  if (number_of_tables_left == 1) break
}

# Find answer for bingo winner table
winner_no <- max(data_Task2$table)

data_Task2b <- data[table == winner_no]

for (i in 1:ncol(data4order)){
  data_Task2b <- data_Task2b[V1 == data4order[1,i], V1 := 0]
  data_Task2b <- data_Task2b[V2 == data4order[1,i], V2 := 0]
  data_Task2b <- data_Task2b[V3 == data4order[1,i], V3 := 0]
  data_Task2b <- data_Task2b[V4 == data4order[1,i], V4 := 0]
  data_Task2b <- data_Task2b[V5 == data4order[1,i], V5 := 0]
  data_Task2b <- data_Task2b[, check := V1 + V2 + V3 + V4 +V5]
  if (prod(data_Task2b$check) == 0) break
}

# Find last calling number
last_call <- data4order[1,i]

# Find other numbers in bingo winner table
winner_no <- data_Task2b[check == 0, table]
winner_table <- data_Task2b[table == winner_no][orient == "Row"]
other_nos <- sum(winner_table$V1) + sum(winner_table$V2) +sum(winner_table$V3) + sum(winner_table$V4) + sum(winner_table$V5)

# Answer
ans2<- last_call * other_nos
