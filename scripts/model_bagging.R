#Cargar librerías 
require("pacman")
p_load(
  ranger,       # Para bagging y random forest
  randomForest, # Para random forest
  tidyverse,    # tidy-data
  caret,        # Entrenamiento del modelo y selección de hiperparámetros.
  Metrics,      # Métricas de evaluación de los problemas de clasificación.
  adabag,
  gbm
)   

train <- read_csv("stores/train.csv")
test  <- read_csv("stores/test.csv")

#omitir missings (solo es una observación)
train <- train %>% na.omit()

fiveStats <- function(...)  c(prSummary(...))

ctrl<- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = fiveStats,
                     classProbs = TRUE,
                     verbose=FALSE,
                     savePredictions = TRUE,
                     sampling = "down")

hyper_grid <- expand.grid(
  mtry = 26,
  min.node.size= c(1, 5, 15, 50), # controla la complejidad del arbol
  splitrule= c('gini','hellinger')
  )  
hyper_grid

set.seed(2025)
cv_bagging <- train(
  Pobre ~., 
  data = train, 
  method = "ranger", # llamamos el paquete del metodo a utilizar
  trControl = ctrl,
  metric="F", # metrica a optimizar
  tuneGrid = hyper_grid,
  ntree = 500,
  importance = 'permutation'
  )

cv_bagging
cv_bagging$finalModel

prob_train <- train |>
  mutate(pobre_lab = predict(cv_bagging, newdata = train, type="prob"))

predictSample <- test |>
  mutate(pobre_lab = predict(cv_bagging, newdata = test, type="prob")) |>
  select(id,pobre_lab) 

#USO DE CURVA ROC PARA ENCONTRAR REGLA DE DECISIÓN ÓPTIMA
train$Pobre <- factor(train$Pobre, levels = c(0, 1), labels = c("No", "Yes"))

prob_train_bag <- predict(cv_bagging, newdata = train, type="prob")

library(pROC)

roc_obj   <- roc(response = train$Pobre, predictor = prob_train_bag)
cut_best  <- coords(roc_obj, "best", ret = "threshold")

predict_test_final <- predictSample %>% 
  mutate(pobre = ifelse(pobre_lab >= 0.2692161, 1, 0)) |>
  select(-pobre_lab)

write.csv(predict_test_final, "RF_mtry_26_node_X_ntree_500_thr_X.csv", row.names = FALSE)

