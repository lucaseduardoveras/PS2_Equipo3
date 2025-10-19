# === Establecer el directorio de trabajo ===
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# === Cargar paquetes ===
require("pacman")
p_load(
  tidyverse, 
  caret,
  naivebayes,
  pROC
)

# === Lectura de datos ===
train <- read_csv("stores/train.csv")
train <- train %>% na.omit()
train$Pobre <- factor(train$Pobre, levels = c("No", "Yes"))

# === Función base para entrenar y obtener curva ROC ===
entrenar_y_roc <- function(sampling_method = NULL, label) {
  
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
    method = "naive_bayes",
    metric = "F",
    trControl = ctrl,
    tuneGrid = expand.grid(
      usekernel = c(TRUE, FALSE),
      laplace = c(0, 1),
      adjust = 1
    )
  )
  
  # Extraer probabilidades y clases verdaderas
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
    AUC = round(auc(roc_obj), 3)
  )
  
  return(roc_df)
}

# === Entrenar los tres modelos ===
roc_base <- entrenar_y_roc(NULL, "Sin muestreo")
roc_up   <- entrenar_y_roc("up", "Upsampling")
roc_down <- entrenar_y_roc("down", "Downsampling")

# === Combinar resultados ===
roc_data <- bind_rows(roc_base, roc_up, roc_down)

# === Gráfico comparativo de curvas ROC ===
ggplot(roc_data, aes(x = FPR, y = TPR, color = Modelo)) +
  geom_line(size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray50") +
  labs(
    title = "Curvas ROC del modelo Naive Bayes con diferentes esquemas de muestreo",
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