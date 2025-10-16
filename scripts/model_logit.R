library(pacman)

p_load("tidyverse",  # Conjunto de paquetes para manipulación, visualización y análisis de datos 
       "caret",       # Herramientas para preprocesamiento, selección de modelos y evaluación de algoritmos de machine learning.
       "MLeval",      # Funciones para evaluar modelos de clasificación y regresión con métricas y gráficos.
       "MLmetrics")   # Colección de métricas de evaluación para modelos de machine learning.

train <- read_csv("stores/train.csv")
test  <- read_csv("stores/test.csv")

modelo_logit <- glm(Pobre ~ . , data = train, family = binomial)
summary(modelo_logit)

prob_train <- train |>
  mutate(pobre_lab = predict(modelo_logit, newdata = train, type = "response")) |>

predictSample <- test |>
  mutate(pobre_lab = predict(modelo_logit, newdata = test, type = "response")) |>
  select(id,pobre_lab) 
head(predictSample)


#vamos a sacar el punto de corte con lo que indique la curva ROC
library(pROC)

roc_obj   <- roc(response = train$Pobre, predictor = prob_train)
cut_best  <- coords(roc_obj, "best", ret = "threshold")

predict_test_final <- predictSample %>% 
  mutate(pobre = ifelse(pobre_lab >= 0.2065815, 1, 0)) |>
  select(-pobre_lab)

write.csv(predict_test_final, "LOGIT.csv", row.names = FALSE)
