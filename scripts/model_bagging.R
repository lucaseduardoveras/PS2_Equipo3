#Cargar librerías 
require("pacman")
p_load(
  ranger,       # Para bagging y random forest
  randomForest, # Para random forest
  tidyverse,    # tidy-data
  caret,        # Entrenamiento del modelo y selección de hiperparámetros.
  Metrics,      # Métricas de evaluación de los problemas de clasificación.
  adabag,
  MLmetrics,
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

test_na <- test %>% na.omit()
predictSample <- test_na |>
  mutate(pobre_lab = predict(cv_bagging, newdata = test_na, type="prob")) |>
  select(id,pobre_lab) 

#USO DE CURVA ROC PARA ENCONTRAR REGLA DE DECISIÓN ÓPTIMA
prob_train_bag <- predict(cv_bagging, newdata = train, type="prob")

library(pROC)
roc_obj   <- roc(response = train$Pobre, predictor = prob_train_bag$Yes)
cut_best  <- coords(roc_obj, "best", ret = "threshold")

# Predecir probabilidades
pred_probs <- predict(cv_bagging, newdata = test_na, type = "prob")

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

#CON F1 PARA PUNTO DE CORTE
# === Calibración del umbral (p) mediante validación cruzada ===
pred_cv <- cv_bagging$pred %>%
  mutate(obs = factor(obs, levels = c("No", "Yes")))

# Definir secuencia de umbrales posibles
thresholds <- seq(0.1, 0.9, by = 0.01)

# Calcular F1-score para cada umbral
f1_scores <- sapply(thresholds, function(t) {
  preds <- ifelse(pred_cv$Yes >= t, "Yes", "No")
  F1_Score(y_pred = preds, y_true = pred_cv$obs, positive = "Yes")
})

# Seleccionar el umbral óptimo
best_p <- thresholds[which.max(f1_scores)]
cat("=== Umbral óptimo (p) encontrado:", best_p, "===\n")

predictSample <- test_na %>%
  mutate(
    pobre_prob = pred_probs$Yes,
    pobre_lab  = if_else(pobre_prob >= 0.63, "Yes", "No")
  ) %>%
  select(id, pobre_lab)

predictSample <- predictSample |> 
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) |>
  select(id, pobre) 

write.csv(predictSample, "RF_mtry_26_node_50_ntree_500_thr_0.63.csv", row.names = FALSE)

