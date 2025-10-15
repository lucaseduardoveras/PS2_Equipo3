library(pacman)

p_load(rio,       # Import/export data.
       tidyverse, # Tidy-data.
       caret,     # For predictive model assessment.
       leaps)     # For subset  model selection

train <- read_csv("stores/train.csv")
test  <- read_csv("stores/test.csv")

#elastic net
train <- train %>% tidyr::drop_na()  
test <- test %>% tidyr::drop_na() 
fiveStats <- function(...)  c(prSummary(...))  

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = fiveStats,
  savePredictions = TRUE
)

set.seed(1234)
model1 <- train(
  Pobre ~ .,
  data = train,
  metric = "F",
  method = "glmnet",
  trControl = ctrl,
  family = "binomial",
  tuneGrid = expand.grid(
    alpha  = seq(0, 1, by= 0.1),
    lambda = 10^seq(-3, 3, length = 10)
  )   
)

model1

predictSample <- test |>
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")) |>
  select(id,pobre_lab)

head(predictSample)

predictSample <- predictSample |> 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) |>
  select(id, pobre)
head(predictSample)

lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(model1$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(model1$bestTune$alpha))

name <- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv"
) 

write.csv(predictSample,name, row.names = FALSE)
