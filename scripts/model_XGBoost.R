#Cargar librerías 
require("pacman")
p_load(
  ranger,       # Para bagging y random forest
  randomForest, # Para random forest
  tidyverse,    # tidy-data
  caret,        # Entrenamiento del modelo y selección de hiperparámetros.
  Metrics,      # Métricas de evaluación de los problemas de clasificación.
  adabag,
  gbm,
  xgboost
)   


train <- read_csv("stores/train.csv")
test  <- read_csv("stores/test.csv")

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter = FALSE, 
  sampling = "down"
)

grid_xbgoost <- expand.grid(nrounds = c(250, 500, 1000),
                            max_depth = c(1,5,15,50),
                            eta = c(0.1,  0.01), 
                            gamma = c(0, 1), 
                            min_child_weight = c(10, 25),
                            colsample_bytree = c(0.4, 0.7), 
                            subsample = c(0.7))
grid_xbgoost

set.seed(123)
xgb_fit_cv <- train(
  x = train[, x_names],
  y = train$Pobre,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = grid_xgboost,
  metric = "ROC",
  verbosity = 0
)
xgb_fit_cv



prob_train_xgb <- predict(xgb_fit_cv, newdata = train, type = "prob")

library(pROC)
roc_obj  <- roc(response = train$Pobre, predictor = prob_train_xgb$Yes,
                levels = c("No","Yes"), direction = "<")
cut_best <- coords(roc_obj, "best", ret = "threshold")

# 2) Predecir probabilidades en test (replicando tu patrón con NA)
test_na    <- test %>% na.omit()
pred_probs <- predict(xgb_fit_cv, newdata = test_na, type = "prob")

# 3) Clasificar según el umbral calibrado por ROC (misma forma que tu snippet)
predictSample <- test_na %>%
  mutate(
    pobre_prob = pred_probs$Yes,
    pobre_lab  = if_else(pobre_prob >= cut_best, "Yes", "No")
  ) %>%
  select(id, pobre_lab)

predictSample <- predictSample %>%
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) %>%
  select(id, pobre)

# 4) Guardar CSV (pon el nombre que quieras; dejo uno corto)
write.csv(predictSample, "XGB_thr_ROCbest.csv", row.names = FALSE)