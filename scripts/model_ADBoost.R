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

#USO DE CURVA ROC PARA ENCONTRAR REGLA DE DECISIÓN ÓPTIMA
prob_train_boost <- predict(adaboost_tree, newdata = train, type="prob")

library(pROC)
roc_obj   <- roc(response = train$Pobre, predictor = prob_train_boost$Yes)
cut_best  <- coords(roc_obj, "best", ret = "threshold")

# Predecir probabilidades
test_na <- test %>% na.omit()
pred_probs <- predict(adaboost_tree, newdata = test_na, type = "prob")

# Clasificar según el umbral calibrado
predictSample <- test_na %>%
  mutate(
    pobre_prob = pred_probs$Yes,
    pobre_lab  = if_else(pobre_prob >= 0.525334, "Yes", "No")
  ) %>%
  select(id, pobre_lab)

predictSample <- predictSample |> 
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) |>
  select(id, pobre) 

write.csv(predictSample, "RF_mtry_26_node_50_ntree_500_thr_0.522.csv", row.names = FALSE)
