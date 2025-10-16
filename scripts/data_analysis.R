library(ggplot2)
library(dplyr)
library(patchwork)
library(readr)


setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

#------------------------------------------------------------
# 1️⃣ Cargar y renombrar datasets (idéntico a tu versión)
#------------------------------------------------------------
train_hogares  <- read_csv("stores/train_hogares.csv")
test_hogares   <- read_csv("stores/test_hogares.csv")

train_hogares <- train_hogares %>%
  rename(
    id_hogar          = id,
    clase_area        = Clase,
    dominio_geo       = Dominio,
    habitaciones_tot  = P5000,
    habitaciones_dorm = P5010,
    tipo_tenencia     = P5090,
    pago_vivienda     = P5100,
    arriendo_imputado = P5130,
    arriendo_efectivo = P5140,
    n_personas        = Nper,
    n_unidad_gasto    = Npersug,
    ingreso_total_ug  = Ingtotug,
    ingreso_total_ug_arr = Ingtotugarr,
    ingreso_pc_ug     = Ingpcug,
    linea_indigencia  = Li,
    linea_pobreza     = Lp,
    es_pobre          = Pobre,
    es_indigente      = Indigente,
    n_pobres          = Npobres,
    n_indigentes      = Nindigentes,
    factor_exp_anual  = Fex_c,
    depto_cod         = Depto,
    factor_exp_depto  = Fex_dpto
  )

test_hogares <- test_hogares %>%
  rename(
    id_hogar          = id,
    clase_area        = Clase,
    dominio_geo       = Dominio,
    habitaciones_tot  = P5000,
    habitaciones_dorm = P5010,
    tipo_tenencia     = P5090,
    pago_vivienda     = P5100,
    arriendo_imputado = P5130,
    arriendo_efectivo = P5140,
    n_personas        = Nper,
    n_unidad_gasto    = Npersug,
    linea_indigencia  = Li,
    linea_pobreza     = Lp,
    factor_exp_anual  = Fex_c,
    depto_cod         = Depto,
    factor_exp_depto  = Fex_dpto
  )

#------------------------------------------------------------
# 2️⃣ Transformaciones y recategorizaciones
#------------------------------------------------------------
train_hogares <- train_hogares %>%
  mutate(
    tipo_tenencia = factor(tipo_tenencia,
                           levels = 1:6,
                           labels = c("Propia_pagada", "Propia_pagando", "Arrendada",
                                      "Usufructo", "Ocupante_hecho", "Otra")),
    es_pobre = factor(es_pobre, labels = c("No pobre", "Pobre")),
    es_indigente = factor(es_indigente, labels = c("No indigente", "Indigente")),
    
    habitaciones_tot = case_when(
      habitaciones_tot >= 4 ~ "4 o más",
      TRUE ~ as.character(habitaciones_tot)
    ),
    habitaciones_dorm = case_when(
      habitaciones_dorm >= 4 ~ "4 o más",
      TRUE ~ as.character(habitaciones_dorm)
    ),
    habitaciones_tot = factor(habitaciones_tot, levels = c("1", "2", "3", "4 o más")),
    habitaciones_dorm = factor(habitaciones_dorm, levels = c("1", "2", "3", "4 o más"))
  )

#------------------------------------------------------------
# 3️⃣ Variables a graficar
#------------------------------------------------------------
vars_categoricas <- c(
  "habitaciones_tot", "habitaciones_dorm",
  "tipo_tenencia", "es_pobre", "es_indigente"
)

vars_numericas <- c(
  "pago_vivienda", "arriendo_imputado", "arriendo_efectivo",
  "n_personas", "n_unidad_gasto"
)

#------------------------------------------------------------
# 4️⃣ Función para pie charts
#------------------------------------------------------------
plot_pie <- function(data, variable) {
  df <- data %>%
    count(!!sym(variable)) %>%
    mutate(prop = n / sum(n),
           label = paste0(round(100 * prop, 1), "%"))
  
  ggplot(df, aes(x = "", y = prop, fill = !!sym(variable))) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5), size = 3) +
    theme_void() +
    theme(legend.position = "none") +  # ocultar leyenda para compactar
    labs(title = variable)
}

#------------------------------------------------------------
# 5️⃣ Función para histogramas
#------------------------------------------------------------
plot_hist <- function(data, variable) {
  ggplot(data, aes_string(x = variable)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    theme_minimal() +
    labs(title = variable, x = NULL, y = NULL)
}

#------------------------------------------------------------
# 6️⃣ Crear una lista con todos los gráficos
#------------------------------------------------------------
plots <- list()

for (v in vars_categoricas) {
  plots[[v]] <- plot_pie(train_hogares, v)
}

for (v in vars_numericas) {
  plots[[v]] <- plot_hist(train_hogares, v)
}

#------------------------------------------------------------
# 7️⃣ Combinar todos los gráficos en una sola figura
#------------------------------------------------------------
combined_plot_hogar <- wrap_plots(plots, ncol = 3) +
  plot_annotation(title = "Distribución de variables del hogar",
                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

#------------------------------------------------------------
# 7️⃣Guardando Gráfico de hogar
#------------------------------------------------------------
ggsave("views/graficos_hogares.png", combined_plot, width = 16, height = 20, dpi = 300)
#------------------------------------------------------------
# Variables a graficar (sin es_pobre ni es_indigente)
#------------------------------------------------------------
vars_categoricas_comp <- c("habitaciones_tot", "habitaciones_dorm", "tipo_tenencia")
vars_numericas_comp <- c(
  "pago_vivienda", "arriendo_imputado", "arriendo_efectivo",
  "n_personas", "n_unidad_gasto", "ingreso_total_ug",
  "ingreso_total_ug_arr", "ingreso_pc_ug",
  "linea_indigencia", "linea_pobreza",
  "factor_exp_anual", "factor_exp_depto"
)


# Subconjuntos
pobres <- filter(train_hogares, es_pobre == "Pobre")
no_pobres <- filter(train_hogares, es_pobre == "No pobre")

# Función auxiliar para sanear nombres de archivo
safe_name <- function(x) gsub("[^A-Za-z0-9_\\-]", "_", tolower(x))

# ------------------------------------------------------------
# Gráficos para variables categóricas: un gráfico con dos paneles (Pobre | No pobre)
# ------------------------------------------------------------
for (v in vars_categoricas_comp) {
  if (!v %in% names(train_hogares)) {
    message("Variable no encontrada, se omite: ", v)
    next
  }
  # niveles ordenados según toda la muestra (para consistencia de colores)
  niveles <- train_hogares %>%
    pull(!!sym(v)) %>%
    as.character() %>%
    unique() %>%
    na.omit()
  niveles <- as.character(niveles)
  
  # Data para pobres y no pobres (con proporciones)
  df_pobre <- pobres %>%
    count(!!sym(v)) %>%
    mutate(prop = n / sum(n),
           label = paste0(round(100 * prop, 1), "%"))
  df_nopobre <- no_pobres %>%
    count(!!sym(v)) %>%
    mutate(prop = n / sum(n),
           label = paste0(round(100 * prop, 1), "%"))
  
  # Asegurar factor con mismos niveles en ambos
  df_pobre[[v]] <- factor(df_pobre[[v]], levels = niveles)
  df_nopobre[[v]] <- factor(df_nopobre[[v]], levels = niveles)
  
  # Paleta (automática) pero consistente entre ambos pies
  pal <- scale_fill_brewer(palette = "Set3", na.value = "grey80", limits = niveles)
  
  p_pobre <- ggplot(df_pobre, aes(x = "", y = prop, fill = !!sym(v))) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
    theme_void() +
    labs(title = paste(v, "— Pobres"), fill = v) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  p_nopobre <- ggplot(df_nopobre, aes(x = "", y = prop, fill = !!sym(v))) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
    theme_void() +
    labs(title = paste(v, "— No pobres"), fill = v) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  # Aplicar la misma escala de colores (paleta) a ambos
  p_pobre <- p_pobre + pal
  p_nopobre <- p_nopobre + pal
  
  # Combinar lado a lado
  combined <- p_pobre + p_nopobre + plot_layout(ncol = 2)
  
  # Guardar
  fname <- paste0("views/fig_", safe_name(v), ".png")
  ggsave(fname, combined, width = 10, height = 5, dpi = 300)
  message("Guardado: ", fname)
}

# ------------------------------------------------------------
# Gráficos para variables numéricas: un gráfico con dos paneles (Pobre | No pobre)
# ------------------------------------------------------------
for (v in vars_numericas_comp) {
  if (!v %in% names(train_hogares)) {
    message("Variable no encontrada, se omite: ", v)
    next
  }
  
  # Rango común para que ambos histogramas usen mismos ejes x
  vals_all <- train_hogares %>% pull(!!sym(v))
  vals_all <- vals_all[!is.na(vals_all)]
  if (length(vals_all) == 0) {
    message("Variable sin datos: ", v, " -> se omite.")
    next
  }
  xlim_min <- min(vals_all, na.rm = TRUE)
  xlim_max <- max(vals_all, na.rm = TRUE)
  
  # Histogramas (mismo formato steelblue)
  p_pobre <- ggplot(pobres, aes_string(x = v)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    coord_cartesian(xlim = c(xlim_min, xlim_max)) +
    theme_minimal() +
    labs(title = paste(v, "— Pobres"), x = NULL, y = NULL) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  p_nopobre <- ggplot(no_pobres, aes_string(x = v)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    coord_cartesian(xlim = c(xlim_min, xlim_max)) +
    theme_minimal() +
    labs(title = paste(v, "— No pobres"), x = NULL, y = NULL) +
    theme(plot.title = element_text(size = 10, face = "bold"))
  
  # Combinar y guardar
  combined <- p_pobre + p_nopobre + plot_layout(ncol = 2)
  fname <- paste0("views/fig_", safe_name(v), ".png")
  ggsave(fname, combined, width = 10, height = 5, dpi = 300)
  message("Guardado: ", fname)
}
