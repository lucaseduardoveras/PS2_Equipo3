setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

library(pacman)

p_load(rio,       # Import/export data.
       tidyverse, # Tidy-data.
       caret,     # For predictive model assessment.
       leaps)     # For subset  model selection

train <- read_csv("stores/train.csv")
test  <- read_csv("stores/test.csv")