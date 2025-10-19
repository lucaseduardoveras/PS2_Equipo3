rm(list=ls())
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
                    savePredictions = TRUE)

grid_gbm <- expand.grid(n.trees= c(50, 100,150),
                        interaction.depth=c(2,5,7),
                        shrinkage=c(0.01, 0.001),
                        n.minobsinnode=c(5, 10))

set.seed(2025) 

gbm_tree <- train(
  Pobre ~.,
  data = train, 
  method = "gbm", 
  trControl = ctrl,
  tuneGrid=grid_gbm,
  metric = "F"
)    
gbm_tree

prob_train <- train |>
  mutate(pobre_lab = predict(gbm_tree, newdata = train, type="prob"))

#USO DE CURVA ROC PARA ENCONTRAR REGLA DE DECISIÓN ÓPTIMA
prob_train_boost <- predict(gbm_tree, newdata = train, type="prob")

library(pROC)
roc_obj   <- roc(response = train$Pobre, predictor = prob_train_boost$Yes)
cut_best  <- coords(roc_obj, "best", ret = "threshold")

# Predecir probabilidades
test_na <- test %>% na.omit()
pred_probs <- predict(gbm_tree, newdata = test_na, type = "prob")

# Clasificar según el umbral calibrado
predictSample <- test_na %>%
  mutate(
    pobre_prob = pred_probs$Yes,
    pobre_lab  = if_else(pobre_prob >= 0.1855536, "Yes", "No")
  ) %>%
  select(id, pobre_lab)

predictSample <- predictSample |> 
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) |>
  select(id, pobre) 

write.csv(predictSample, "GBM_ntree_150_depth_7_shri_0.01_node_5.csv", row.names = FALSE)
