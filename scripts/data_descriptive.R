# === Setup ===
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

require("pacman")
p_load(tidyverse, stargazer)

# === Read and clean data ===
train <- read_csv("stores/train.csv") |> na.omit()

# Ensure Pobre is a factor with correct labels
train <- train |> 
  mutate(Pobre = factor(Pobre, levels = c("Yes", "No"), labels = c("Pobre", "No Pobre")))

# === Split the dataset ===
train_pobre    <- train %>% filter(Pobre == "Pobre")
train_nopobre  <- train |> filter(Pobre == "No Pobre")

# === Select only numeric variables ===
numeric_vars <- train |> select(where(is.numeric)) |> names()

# Subset numeric data for each group
num_pobre    <- train_pobre  |> select(all_of(numeric_vars))
num_nopobre  <- train_nopobre |> select(all_of(numeric_vars))

num_pobre_df   <- as.data.frame(num_pobre)
num_nopobre_df <- as.data.frame(num_nopobre)

factor_vars <- train |> select(-where(is.numeric)) |> names()
factor_vars <- c("bin_headWoman","bin_headSS","bin_occupiedHead", "bin_headCpropia",factor_vars)

# Generate descriptive table
stargazer(num_pobre_df,
          type = "text",   # or "latex" for export
          title = "Estadísticas descriptivas por condición de pobreza",
          column.labels = c("Pobres", "No pobres"),
          summary.stat = c("mean", "median", "sd"),
          digits = 2)

stargazer(num_nopobre_df,
          type = "text",   # or "latex" for export
          title = "Estadísticas descriptivas por condición de pobreza",
          column.labels = c("Pobres", "No pobres"),
          summary.stat = c("mean", "median", "sd"),
          digits = 2)

fact_pobre    <- train_pobre  |> select(all_of(factor_vars))

# === Convertir a factor las variables indicadas ===
fact_pobre <- fact_pobre %>%
  mutate(across(
    c(
      Dominio, Pobre, tipo_vivienda, cat_educHead, cat_maxEduc),
    as.factor
  ))

# === Crear dummies (una por categoría, excepto 'Dominio') ===
fact_pobre_dummy <- dummyVars(
  ~ tipo_vivienda + cat_educHead + cat_maxEduc,
  data = fact_pobre,
  fullRank = FALSE  # <-- genera una dummy por cada categoría
)

# === Generar las dummies ===
train_dummies <- predict(fact_pobre_dummy, newdata = fact_pobre) %>%
  as.data.frame() %>%
  janitor::clean_names()  # Limpia nombres (sin espacios ni símbolos)

# === Unir las dummies al dataset original ===
fact_pobre <- bind_cols(fact_pobre, train_dummies)

fact_pobre <- as.data.frame(fact_pobre) 
# Solo promedio
stargazer(fact_pobre,
          type = "text",
          summary.stat = c("mean"),
          title = "Promedios de variables categóricas Pobres")

# Lo mismo anterior para No Pobre

fact_nopobre    <- train_nopobre  |> select(all_of(factor_vars))

# === Convertir a factor las variables indicadas ===
fact_nopobre <- fact_nopobre %>%
  mutate(across(
    c(
      Dominio, Pobre, tipo_vivienda, cat_educHead, cat_maxEduc),
    as.factor
  ))

# === Crear dummies (una por categoría, excepto 'Dominio') ===
fact_nopobre_dummy <- dummyVars(
  ~ tipo_vivienda + cat_educHead + cat_maxEduc,
  data = fact_nopobre,
  fullRank = FALSE  # <-- genera una dummy por cada categoría
)

# === Generar las dummies ===
train_dummies <- predict(fact_nopobre_dummy, newdata = fact_nopobre) %>%
  as.data.frame() %>%
  janitor::clean_names()  # Limpia nombres (sin espacios ni símbolos)

# === Unir las dummies al dataset original ===
fact_nopobre <- bind_cols(fact_nopobre, train_dummies)

fact_nopobre <- as.data.frame(fact_nopobre) 
# Solo promedio
stargazer(fact_nopobre,
          type = "text",
          summary.stat = c("mean"),
          title = "Promedios de variables categóricas NoPobres")

#===Diferencia de medias===#
library(dplyr)
library(broom)
library(purrr)
library(kableExtra)

# Variables a comparar
train$Pobre <- ifelse(train$Pobre %in% c("Sí", "Si", "YES", "Yes", "Pobre"), 1, 0)
vars <- train %>% select(-Pobre) %>% select(where(is.numeric)) %>% names()

# Tabla de diferencias de medias
diff_means <- vars %>%
  map_dfr(~ {
    var <- .x
    ttest <- t.test(train[[var]] ~ train$Pobre)
    tibble(
      Variable = var,
      Media_Pobre = mean(train[[var]][train$Pobre == 1], na.rm = TRUE),
      Media_NoPobre = mean(train[[var]][train$Pobre == 0], na.rm = TRUE),
      Diferencia = diff(ttest$estimate),   # Pobre - No pobre
      t = unname(ttest$statistic),
      p_valor = ttest$p.value
    )
  }) %>%
  mutate(Signif = case_when(
    p_valor < 0.01 ~ "***",
    p_valor < 0.05 ~ "**",
    p_valor < 0.1  ~ "*",
    TRUE ~ ""
  ))

# Mostramos la tabla en R
diff_means %>%
  mutate(across(c(Media_Pobre, Media_NoPobre, Diferencia, t, p_valor), round, 3)) %>%
  kbl(caption = "Diferencia de medias entre hogares pobres y no pobres") %>%
  kable_styling(full_width = FALSE)

write.csv(diff_means, "diferencias_medias.csv", row.names = FALSE)

