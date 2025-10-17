# === Establecer el directorio de trabajo ===
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# === Cargar paquetes ===
require("pacman")
p_load(
  tidyverse, 
  caret,
  naivebayes,   # Modelo Naive Bayes
  MLmetrics, 
  Metrics
)

# === Lectura de datos ===
train <- read_csv("stores/train.csv")
test  <- read_csv("stores/test.csv")

# Eliminar observaciones con NA en el conjunto de entrenamiento
train <- train %>% na.omit()

# Asegurar que la variable objetivo sea factor con niveles consistentes
train$Pobre <- factor(train$Pobre, levels = c("No", "Yes"))

# === Configuración de la validación cruzada con UPSAMPLING ===
fiveStats <- function(...)  c(prSummary(...))  

ctrl <- trainControl(
  method = "cv",             # Validación cruzada
  number = 5,                # 5 pliegues
  classProbs = TRUE,         # Calcular probabilidades
  summaryFunction = fiveStats,
  savePredictions = TRUE,    # Guardar predicciones para calibrar el umbral
  sampling = "down"            # <-- Downsampling de la clase minoritaria
)

# === Entrenamiento del modelo Naive Bayes ===
set.seed(2025)
model1 <- train(
  Pobre ~ .,                
  data = train,
  method = "naive_bayes",
  metric = "F",             # Métrica base para tuning interno
  trControl = ctrl,
  tuneGrid = expand.grid(
    usekernel = c(TRUE, FALSE),
    laplace = c(0, 1),
    adjust = 1
  )
)

# === Calibración del umbral (p) mediante validación cruzada ===
pred_cv <- model1$pred %>%
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

# Gráfico opcional de calibración
plot(thresholds, f1_scores, type = "l", lwd = 2,
     xlab = "Umbral (p)", ylab = "F1-score",
     main = "Calibración del Umbral")

# === Predicciones finales en el conjunto de test ===
prob <- test[which(!complete.cases(test)),]
id_prob <- prob$id

# Eliminar NAs antes de predecir
test <- test %>% na.omit()

# Predecir probabilidades
pred_probs <- predict(model1, newdata = test, type = "prob")

# Clasificar según el umbral calibrado
predictSample <- test %>%
  mutate(
    pobre_prob = pred_probs$Yes,
    pobre_lab  = if_else(pobre_prob >= best_p, "Yes", "No")
  ) %>%
  select(id, pobre_lab)

# Añadir las observaciones faltantes (NA) como "Yes"
prob_lab <- data.frame(id = id_prob, pobre_lab = "Yes")

# Combinar resultados finales
predictSample <- bind_rows(predictSample, prob_lab)

predictSample <- predictSample |> 
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) |>
  select(id, pobre) 

# === Exportar resultados ===
write.csv(predictSample, "stores/modelos/naive_bayes_downsampling.csv", row.names = FALSE)