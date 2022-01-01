# Day 10

# Packages ----------------------------------------------------------------
library(dplyr)
library(stringi)

# Data --------------------------------------------------------------------
data10 <-  as.data.frame(read.csv("raw_inputs//input_Day10.txt", header=FALSE, colClasses = "character"))

# Task 1 ------------------------------------------------------------------

data <- data10

# Function that deletes the brackets in the problem if they are in a pair next to each other
bracket_del <- function(string){
  return(str_replace_all(string, c("\\(\\)" = "", 
                                   "\\{\\}" = "",
                                   "\\[\\]" = "",
                                   "<>" = "")))}

# Function that finds the first element in a string that is either a ], }, ) or >
first_close_bracket <- function(string){
  string <- str_replace_all(string, c("\\(" = "", 
                                      "\\{" = "",
                                      "\\[" = "",
                                      "<" = ""))
  return(as.data.frame(substring(string, 1, 1)))}
  
# Remove all paired brackets until no more can be deleted
for (i in 1:nrow(data)){
  repeat{   
    dummy1 <- nchar(data[i,1])
    data[i,1] <- bracket_del(data[i,1])
    dummy2 <- nchar(data[i,1])
    if (dummy1 == dummy2){
      break
    }}}

# Find first closing bracket in remaining string
first_bracket <- first_close_bracket(data[,1])
names(first_bracket) <- c("Bracket")

# Find answer
first_bracket$score <- ifelse(first_bracket$Bracket==")", 3, 
                              ifelse(first_bracket$Bracket=="]", 57, 
                                     ifelse(first_bracket$Bracket=="}", 1197, 
                                            ifelse(first_bracket$Bracket==">", 25137, 0))))
ans1 <- sum(first_bracket$score)


# Task 2 ------------------------------------------------------------------

# Filter out the corrupt lines
data$V2 <- first_close_bracket(data[,1])
data <- filter(data, data$V2 == "")

# Reverse strings
data$V2 <- stringi::stri_reverse(data$V1)

# Replace open brackets with closed brackets
data$V3 <- str_replace_all(data$V2, c("\\(" = ")", 
                          "\\{" = "}",
                          "\\[" = "]",
                          "<" = ">"))

# Score each line

for (i in 1:nrow(data)){
  score <- 0
  for (j in 1:nchar(data[i,3])){
    bracket <- substring(data[i,3], j, j)
    if(bracket == ")"){score_character <- 1}
    if(bracket == "]"){score_character <- 2}
    if(bracket == "}"){score_character <- 3}
    if(bracket == ">"){score_character <- 4}
    score <- (5 * score) + score_character
  }
  data$V4[i] <- score
}

# Calculate answer
ans2 <- median(data$V4)
