# === Establecer el directorio de trabajo ===
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# === Cargar paquetes ===
require("pacman")
p_load(
  tidyverse, 
  caret,
  glmnet,
  pROC
)

# === Lectura de datos ===
train <- read_csv("stores/train.csv")
train <- train %>% na.omit()
train$Pobre <- factor(train$Pobre, levels = c("No", "Yes"))

# === Función base para entrenar y obtener curva ROC + mejores hiperparámetros ===
entrenar_y_roc_elnet <- function(sampling_method = NULL, label) {
  
  fiveStats <- function(...) c(prSummary(...))
  
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = fiveStats,
    savePredictions = TRUE,
    sampling = sampling_method  # puede ser "up", "down" o NULL
  )
  
  set.seed(2025)
  model <- train(
    Pobre ~ .,
    data = train,
    method = "glmnet",
    family = "binomial",
    metric = "F",
    trControl = ctrl,
    tuneGrid = expand.grid(
      alpha = seq(0, 1, by = 0.1),
      lambda = 10^seq(-4, 1, length = 20)
    )
  )
  
  # === Guardar los mejores hiperparámetros ===
  best_params <- model$bestTune
  cat("=== Mejores hiperparámetros para", label, "===\n")
  print(best_params)
  
  # === Calcular curva ROC ===
  preds <- model$pred
  roc_obj <- roc(
    response = preds$obs,
    predictor = preds$Yes,
    levels = c("No", "Yes"),
    direction = "<"
  )
  
  roc_df <- data.frame(
    TPR = roc_obj$sensitivities,
    FPR = 1 - roc_obj$specificities,
    Modelo = label,
    AUC = round(auc(roc_obj), 3),
    alpha = best_params$alpha,
    lambda = best_params$lambda
  )
  
  return(roc_df)
}

# === Entrenar los tres modelos ===
roc_base <- entrenar_y_roc_elnet(NULL, "Sin muestreo")
roc_up   <- entrenar_y_roc_elnet("up", "Upsampling")
roc_down <- entrenar_y_roc_elnet("down", "Downsampling")

# === Combinar resultados ===
roc_data <- bind_rows(roc_base, roc_up, roc_down)

# === Guardar los hiperparámetros óptimos de cada modelo ===
best_hyperparams <- roc_data %>%
  select(Modelo, alpha, lambda, AUC) %>%
  distinct()

write_csv(best_hyperparams, "views/ElasticNet_best_hyperparams.csv")

# === Gráfico comparativo de curvas ROC ===
fig_roc <- ggplot(roc_data, aes(x = FPR, y = TPR, color = Modelo)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50") +
  labs(
    title = "Curvas ROC del modelo Elastic Net con diferentes esquemas de muestreo",
    x = "Tasa de Falsos Positivos (1 - Especificidad)",
    y = "Tasa de Verdaderos Positivos (Sensibilidad)",
    color = "Método"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# === Calcular e imprimir AUC promedio ===
roc_data %>%
  group_by(Modelo) %>%
  summarise(AUC = unique(AUC))

# === Guardar gráfico ===
ggsave("views/ROC_ElasticNet_sampling.png", fig_roc, width = 8, height = 6, dpi = 300)
