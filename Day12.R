# Day 12

# Packages ----------------------------------------------------------------
library(dplyr)

# Data --------------------------------------------------------------------
#data12 <-  as.data.frame(read.csv("raw_inputs//input_Day12_test.txt", header=FALSE, colClasses = "character"))
data12 <-  as.data.frame(read.csv("raw_inputs//input_Day12.txt", header=FALSE, colClasses = "character"))
data12 <- strsplit(as.character(data12$V1), split = "-")
data12 <- unlist(data12)
data12 <- as.data.frame(matrix(data12, ncol = 2, byrow = TRUE), stringsAsFactors=FALSE)


# Task 1 ------------------------------------------------------------------
# make lists of all nodes and lowercase nodes
data <- data12
data <- rbind(data, data %>% rename(V1 = V2, V2 = V1))
cave_list <- unique(data$V1)
lowercase_caves <- cave_list[!grepl("[[:upper:]]", cave_list)]

# function that looks for all possible caves next
find_next_cave <- function(data, path_so_far){
  current_loc <- path_so_far[length(path_so_far)]
  if (current_loc == "end"){
    return(c())
  } else {
    all_possibilities <- data %>% filter(V1 == current_loc) %>% pull(V2)
    viable_possibilities <- all_possibilities %>% .[!(. %in% path_so_far & . %in% lowercase_caves)]
    return(viable_possibilities)}}

# function that records the paths as they progress
update_paths <- function(all_paths, path_so_far, viable_possibilities, i){
  all_paths <- all_paths[-i]
  new_paths <- lapply(viable_possibilities %>% as.list(), function(x){c(path_so_far, x)})
  all_paths <- c(all_paths, new_paths)
  return(all_paths)}

# iterate through all the path options
# i is used as a counter that only changes when paths have been exhausted in current path
# new paths are added at the end so i always looks for routes that haven't just been looked at
i <- 1
all_paths <- list(c("start"))

while(i <= length(all_paths)){
  path_so_far <- all_paths[i] %>% unlist()
  viable_possibilities <- find_next_cave(data, path_so_far)
  # print(path_so_far)
  # print(viable_possibilities)
  if (length(viable_possibilities) > 0){
    all_paths <- update_paths(all_paths, path_so_far, viable_possibilities, i)
  } else {
    i <- i+1}}

# count how many of the paths don't lead to dead ends in small caves
ans1 <- 0
for (i in 1:length(all_paths)){
  if (all_paths[[i]][length(all_paths[[i]])] == "end"){
    ans1 <- ans1 + 1}}

# Task 2 ------------------------------------------------------------------

# function that looks for all possible caves next
find_next_cave_2 <- function(data, path_so_far){
  current_loc <- path_so_far[length(path_so_far)]
  if (current_loc == "end"){
    return(c())
  } else {
    all_possibilities <- data %>% filter(V1 == current_loc) %>% pull(V2)
    twiced <- path_so_far %>% .[duplicated(.)] %>% .[. %in% lowercase_caves] %>% length() %>% as.logical()
    if (twiced == TRUE){
      viable_possibilities <- all_possibilities %>% .[!(. %in% path_so_far & . %in% lowercase_caves)]
    } else {
      viable_possibilities <- all_possibilities %>% .[!(. %in% c("start"))]
    }
  }
  return(viable_possibilities)
}

# function that records the paths as they progress
update_paths <- function(all_paths, path_so_far, viable_possibilities, i){
  all_paths <- all_paths[-i]
  new_paths <- lapply(viable_possibilities %>% as.list(), function(x){c(path_so_far, x)})
  all_paths <- c(all_paths, new_paths)
  return(all_paths)}

# iterate through all the path options
# i is used as a counter that only changes when paths have been exhausted in current path
# new paths are added at the end so i always looks for routes that haven't just been looked at
i <- 1
all_paths <- list(c("start"))

while(i <= length(all_paths)){
  path_so_far <- all_paths[i] %>% unlist()
  viable_possibilities <- find_next_cave_2(data, path_so_far)
  # print(path_so_far)
  # print(viable_possibilities)
  if (length(viable_possibilities) > 0){
    all_paths <- update_paths(all_paths, path_so_far, viable_possibilities, i)
  } else {
    i <- i+1
    if (i%% 10000 == 0){
      print(i)
      }}}

# count how many of the paths don't lead to dead ends in small caves that can't be escaped
ans2 <- 0
for (i in 1:length(all_paths)){
  if (all_paths[[i]][length(all_paths[[i]])] == "end"){
    ans2 <- ans2 + 1}}
