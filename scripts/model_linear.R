library(pacman)

p_load("tidyverse",  # Conjunto de paquetes para manipulación, visualización y análisis de datos 
       "caret",       # Herramientas para preprocesamiento, selección de modelos y evaluación de algoritmos de machine learning.
       "MLeval",      # Funciones para evaluar modelos de clasificación y regresión con métricas y gráficos.
       "MLmetrics")   # Colección de métricas de evaluación para modelos de machine learning.

train <- read_csv("stores/train.csv")
test  <- read_csv("stores/test.csv")

train$Pobre <- ifelse(train$Pobre == "Yes", 1, 0)
modelo_mpl <- lm(Pobre ~ . + I(Nper^2) + I(edad_head^2) +
                   bin_headWoman:cat_educHead + edad_head:cat_educHead +
                   Nper:t_dependencia, data = train)
summary(modelo_mpl)

prob_train <- train |>
  mutate(pobre_lab = predict(modelo_mpl, newdata = train))
  
  predictSample <- test |>
  mutate(pobre_lab = predict(modelo_mpl, newdata = test)) |>
  select(id,pobre_lab) 

head(prob_train)


#vamos a sacar el punto de corte con lo que indique la curva ROC
train$Pobre <- factor(train$Pobre, levels = c(0, 1), labels = c("No", "Yes"))
prob_train_mpl <- predict(modelo_lpm, newdata = train)
library(pROC)

roc_obj   <- roc(response = train$Pobre, predictor = prob_train_mpl)
cut_best  <- coords(roc_obj, "best", ret = "threshold")

predict_test_final <- predictSample %>% 
  mutate(pobre = ifelse(pobre_lab >= 0.2692161, 1, 0)) |>
  select(-pobre_lab)

write.csv(predict_test_final, "LINEAR.csv", row.names = FALSE)
