setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))


require("pacman")
p_load(tidyverse, 
       glmnet,
       caret,
       MLmetrics, 
       Metrics
)

train <- read_csv("stores/train.csv")
test  <- read_csv("stores/test.csv")

train <- train %>% na.omit()
 

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = TRUE
)

fiveStats <- function(...)  c(prSummary(...))  

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = fiveStats,
  savePredictions = TRUE
)

set.seed(2025)
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

# Tomando la id de la obs de test problemática

prob <- test[which(!complete.cases(test)),]
id_prob <- prob$id

test <- test %>% na.omit()

predictSample <- test |>
  mutate(pobre_lab = predict(model1, newdata = test, type = "raw")) |>
  select(id,pobre_lab)

prob_lab <- data.frame(id = id_prob, pobre_lab = "Yes")

predictSample <- rbind(predictSample, prob_lab)

predictSample <- predictSample |> 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) |>
  select(id, pobre) 


lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(model1$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(model1$bestTune$alpha))

name <- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv"
) 

write.csv(predictSample,paste("stores/modelos/",name,sep = ""), row.names = FALSE)
