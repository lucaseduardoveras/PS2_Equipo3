### Random Forest ###

setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# 0) Se cargan los paquetes importantes

library(caret)
library(recipes)
library(ranger)
library(dplyr)
library(readr)
library(tibble)
library(pROC)
library(MLmetrics)

set.seed(123)

# 1) Se leen las bases de datos

train <- readr::read_csv("stores/train.csv", show_col_types = FALSE)
test  <- readr::read_csv("stores/test.csv",  show_col_types = FALSE)

# 3) Se prepara de forma:

if (!("Pobre" %in% names(train))) stop("No se encontró la variable objetivo 'Pobre'.")
train <- train %>% mutate(Pobre = factor(Pobre, levels = c("No", "Yes")))

x_names <- setdiff(names(train), "Pobre")

# Se identifican las categóricas por nombres

to_factor <- grep("^(bin_|cat_|Dominio$)", x_names, value = TRUE)

# Se convierten a factores

train <- train %>% mutate(across(all_of(to_factor), as.factor))
test  <- test  %>% mutate(across(all_of(to_factor), as.factor))

# Se alinean niveles

for (v in to_factor) {
  if (v %in% names(test)) {
    lvls <- union(levels(as.factor(train[[v]])), levels(as.factor(test[[v]])))
    train[[v]] <- factor(train[[v]], levels = lvls)
    test[[v]]  <- factor(test[[v]],  levels = lvls)
  }
}

# 4) Control de entrenamiento (CV=5, ROC)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter = FALSE
)

# 5) Se construye el grid de hiperparámetros

grid_rf <- expand.grid(
  mtry = c(6, 8, 9, 10),
  splitrule = "gini",
  min.node.size = 1
)

# 6) Se hace entrenamiento CV con 500 árboles

set.seed(123)
rf_fit_cv <- train(
  x = train[, x_names],
  y = train$Pobre,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = grid_rf,
  importance = "impurity",
  num.trees = 500,
  metric = "ROC"
)

# 7) Se hace un modelo final con el mejor mtry

best_mtry <- rf_fit_cv$bestTune$mtry
final_grid <- expand.grid(
  mtry = best_mtry,
  splitrule = "gini",
  min.node.size = 1
)

set.seed(123)
rf_final <- train(
  x = train[, x_names],
  y = train$Pobre,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = final_grid,
  importance = "impurity",
  num.trees = 1000,
  metric = "ROC"
)

# 8) Umbral óptimo (ROC y F1) a partir del CV

# ROC

roc_obj <- roc(response = rf_fit_cv$pred$obs,
               predictor = rf_fit_cv$pred$Yes,
               levels = c("No", "Yes"),
               direction = "<")

best_thr_roc <- as.numeric(coords(roc_obj, "best", ret = "threshold", transpose = FALSE))

# F1

thresholds <- seq(0.05, 0.95, by = 0.01)
f1_scores <- sapply(thresholds, function(t) {
  preds_bin <- ifelse(rf_fit_cv$pred$Yes >= t, "Yes", "No")
  F1_Score(y_pred = preds_bin, y_true = rf_fit_cv$pred$obs, positive = "Yes")
})
best_thr_f1 <- thresholds[which.max(f1_scores)]

# Se selecciona alguno de los dos

threshold_final <- best_thr_roc
cat("\nUmbral óptimo ROC:", round(best_thr_roc, 4),
    " | Umbral óptimo F1:", round(best_thr_f1, 4),
    " | F1(CV) máx:", round(max(f1_scores), 3), "\n")

# 9) Importancia de variables
dir.create("rf_outputs", showWarnings = FALSE)
var_imp <- varImp(rf_final, scale = TRUE)

imp_tbl <- var_imp$importance %>%
  rownames_to_column("variable") %>%
  arrange(desc(Overall)) %>%
  slice(1:30)

write_csv(imp_tbl, "rf_outputs/variable_importance_top30.csv")

# 10) Se hace un output con la predicción

id_col <- if ("id" %in% names(test)) "id" else if ("Id" %in% names(test)) "Id" else NA
if (is.na(id_col)) stop("No hay ID")

pred_prob <- predict(rf_final, newdata = test[, x_names], type = "prob")[, "Yes"]

pred_bin  <- ifelse(pred_prob >= threshold_final, 1, 0)

submission <- tibble(
  id = test[[id_col]],
  pobre = pred_bin
)

file_name <- sprintf("RF_mtry_%s_ntree_1000_thr_%.3f_ROC.csv", best_mtry, threshold_final)

write_csv(submission, file.path("rf_outputs", file_name))