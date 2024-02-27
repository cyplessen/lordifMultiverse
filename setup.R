# Readme

# library(lordif)
# library(tidyverse)
# library(lordifMultiverse)
# data(Anxiety)
# items <- Anxiety[paste("R",1:29,sep="")]
# group_age <- Anxiety$age
group_gender <- Anxiety$gender
specification_results <- lordif_multiverse(items_anxiety, group_gender)
