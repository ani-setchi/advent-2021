# Day 6

# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
data6 <-  as.data.frame(read.table("raw_inputs//input_Day6.txt", header=FALSE))
data <- as.data.frame(as.integer(t(do.call('rbind', strsplit(as.character(data6$V1),',')))))
colnames(data) <- c("int_timer")


# Task 1 ------------------------------------------------------------------
# Create a frequency table
freq <- data %>%
  count(int_timer) %>% 
  group_by(int_timer)
# Create a vector to store number of each type of lantern fish
counting <- as.data.frame(matrix(0, nrow = 1, ncol = 9))
colnames(counting) <- c("s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8")
for (i in 1:nrow(freq)){
  counting[1, freq$int_timer[i] + 1] <- freq$n[i]
}
# Iteratre through the days
for (i in 1:80){
  baby <- counting$s0
  for(j in 2:9){
    counting[1, j-1] <- counting[1, j]
  }
  counting[7] <- counting[7] + baby # both 0s and 7s become 6s the next day
  counting[9] <- baby # all new 8s were 0s the day before
}
# Calculate answer
ans1 <- sum(counting)


# Task 2 ------------------------------------------------------------------
# Create a vector to store number of each type of lantern fish
counting <- as.data.frame(matrix(0, nrow = 1, ncol = 9))
colnames(counting) <- c("s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8")
for (i in 1:nrow(freq)){
  counting[1, freq$int_timer[i] + 1] <- freq$n[i]
}
# Iteratre through the days
for (i in 1:256){
  baby <- counting$s0
  for(j in 2:9){
    counting[1, j-1] <- counting[1, j]
  }
  counting[7] <- counting[7] + baby # both 0s and 7s become 6s the next day
  counting[9] <- baby # all new 8s were 0s the day before
}
# Calculate answer
ans2 <- sum(counting)
