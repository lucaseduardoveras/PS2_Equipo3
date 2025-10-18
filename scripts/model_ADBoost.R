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

adagrid <- expand.grid(mfinal = c(250, 500, 1000),
                       maxdepth = c(1,5,15,50),
                       coeflearn = c('Breiman','Freund'))

set.seed(2025) 

adaboost_tree <- train(
  Pobre ~.,
  data = train, 
  method = "AdaBoost.M1",  # para implementar el algoritmo antes descrito
  trControl = ctrl,
  metric = "F",
  tuneGrid = adagrid
)
adaboost_tree

prob_train <- train |>
  mutate(pobre_lab = predict(adaboost_tree, newdata = train, type="prob"))

predictSample <- test |>
  mutate(pobre_lab = predict(adaboost_tree, newdata = test, type="prob")) |>
  select(id,pobre_lab) 

#USO DE CURVA ROC PARA ENCONTRAR REGLA DE DECISIÓN ÓPTIMA
train$Pobre <- factor(train$Pobre, levels = c(0, 1), labels = c("No", "Yes"))

prob_train_boost <- predict(adaboost_tree, newdata = train, type="prob")

library(pROC)

roc_obj   <- roc(response = train$Pobre, predictor = prob_train_boost)
cut_best  <- coords(roc_obj, "best", ret = "threshold")

predict_test_final <- predictSample %>% 
  mutate(pobre = ifelse(pobre_lab >= 0.2692161, 1, 0)) |>
  select(-pobre_lab)

write.csv(predict_test_final,
          "BOOST_node_X_ntree_x_coef_X_thr_X.csv", row.names = FALSE)