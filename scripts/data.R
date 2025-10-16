setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

require("pacman")
p_load(tidyverse, 
       glmnet,
       caret,
       MLmetrics, 
       Metrics,
       readr
)

train_hogares  <- read_csv("stores/train_hogares.csv")
train_personas <- read_csv("stores/train_personas.csv")
test_hogares  <- read_csv("stores/test_hogares.csv")
test_personas <- read_csv("stores/test_personas.csv")

#limpieza y renombramiento de variables en la base de personas

train_personas<- train_personas |> 
  mutate(bin_woman = ifelse(P6020==2,1,0), 
         bin_head = ifelse(P6050== 1, 1, 0),
         bin_minor = ifelse(P6040<=15,1,0),
         bin_mayor = ifelse(P6040>=65,1,0),
         bin_educ_sup = ifelse(P6210==6, 1, 0),
         cat_educ = ifelse(P6210==9,0,P6210), # La categoría 9 no añade valor.
         bin_occupied = ifelse(is.na(Oc),0,1),
         bin_inac = ifelse(is.na(Ina), 0, 1),
         P6800 = ifelse(is.na(P6800), 0, P6800),
         bin_c_propia = ifelse(!is.na(P6430) & P6430 == 4, 1, 0),
         bin_social_sec = ifelse(P6090==1, 1, 0)) |> 
  rename(edad = P6040,
         pet = Pet,
         ocu = Oc,
         inac = Ina,
         horas_trabajadas = P6800) |>
  select(id, Orden, bin_woman, bin_head, bin_minor, bin_mayor, cat_educ, bin_occupied, edad,
         bin_social_sec, pet, ocu, inac, bin_c_propia, bin_educ_sup, bin_inac, horas_trabajadas)

#variables desde base personas a hogar
#estas variables son la suma, la tasa o el maximo de variables de interes

train_vars_personas_hogar <- train_personas |> 
  group_by(id) |>
  summarize(num_women    = sum(bin_woman, na.rm = TRUE),
            num_minors   = sum(bin_minor, na.rm = TRUE),
            num_mayores = sum(bin_mayor, na.rm = TRUE),
            num_dep = num_minors + num_mayores,
            cat_maxEduc  = max(cat_educ, na.rm = TRUE),
            num_educ_sup = sum(bin_educ_sup, na.rm = TRUE),
            num_c_propia = sum(bin_c_propia, na.rm = TRUE),
            num_h_trabajadas = sum(horas_trabajadas, na.rm = TRUE),
            num_ocu = sum(bin_occupied, na.rm = TRUE),
            num_pet = sum(pet, na.rm = TRUE),
            num_inac = sum(inac, na.rm=TRUE)) |> 
  mutate(t_ocu = num_ocu/num_pet,
         t_inac = num_inac/num_pet,
         t_educ_sup = num_educ_sup/num_pet,
         t_dependencia = if_else(num_pet > 0, 100 * num_dep / num_pet, NA_real_),
         t_c_propia = num_c_propia/num_pet) |>
  ungroup()
train_vars_personas_hogar <- train_vars_personas_hogar |> 
  select(-num_dep) #por multicolinealidad

#variables por jefe del hogar
train_jefe_hogar <- train_personas |> 
  filter(bin_head == 1) |>
  select(id, bin_woman, cat_educ, bin_social_sec, bin_occupied, edad, bin_c_propia,
         horas_trabajadas) |>
  rename(bin_headWoman = bin_woman,
         cat_educHead = cat_educ,
         edad_head = edad,
         bin_headSS = bin_social_sec,
         bin_headCpropia = bin_c_propia,
         h_workedHead = horas_trabajadas,
         bin_occupiedHead = bin_occupied) |>
  left_join(train_vars_personas_hogar, by = "id")

#variables de la base a nivel hogar
train_hogares <- train_hogares |>
  mutate(P5090 = ifelse(P5090==6,0,P5090)) |>
    rename(n_habitaciones = P5000,
           tipo_vivienda = P5090) |>
  select(id, Dominio, Pobre, Nper, Lp, n_habitaciones, tipo_vivienda)

#base definitiva
train <- train_hogares |> 
  left_join(train_jefe_hogar) |>
  select(-id) # Ya no necesitamos el identificador.

#categóricas a factores
train <- train |> 
  mutate(Pobre = factor(Pobre,levels=c(0,1),labels=c("No","Yes")),
         Dominio = factor(Dominio),
         cat_maxEduc = factor(
           cat_maxEduc, 
           levels = c(0:6),
           labels=c("No sabe",'Ninguno', 'Preescolar', 'Primaria',
                    'Secundaria', 'Media', 'Universitaria')),
         cat_educHead = factor(
           cat_educHead, 
           levels = c(0:6),
           labels=c("No sabe",'Ninguno', 'Preescolar', 'Primaria',
                    'Secundaria', 'Media', 'Universitaria')),
        tipo_vivienda = factor(
          tipo_vivienda, 
          levels = c(0:5),
          labels=c("Otra", 'Propia, totalmente pagada','Propia, la están pagando',
                   'En arriendo o subarriendo', 'En usufructo',
                   'Posesión sin titulo')))

#####Ahora replicamos el proceso para el test set#####

#limpieza y renombramiento de variables en la base de personas

test_personas<- test_personas |> 
  mutate(bin_woman = ifelse(P6020==2,1,0), 
         bin_head = ifelse(P6050== 1, 1, 0),
         bin_minor = ifelse(P6040<=15,1,0),
         bin_mayor = ifelse(P6040>=65,1,0),
         bin_educ_sup = ifelse(P6210==6, 1, 0),
         cat_educ = ifelse(P6210==9,0,P6210), # La categoría 9 no añade valor.
         bin_occupied = ifelse(is.na(Oc),0,1),
         bin_inac = ifelse(is.na(Ina), 0, 1),
         P6800 = ifelse(is.na(P6800), 0, P6800),
         bin_c_propia = ifelse(!is.na(P6430) & P6430 == 4, 1, 0),
         bin_social_sec = ifelse(P6090==1, 1, 0)) |> 
  rename(edad = P6040,
         pet = Pet,
         ocu = Oc,
         inac = Ina,
         horas_trabajadas = P6800) |>
  select(id, Orden, bin_woman, bin_head, bin_minor, bin_mayor, cat_educ, bin_occupied, edad,
         bin_social_sec, pet, ocu, inac, bin_c_propia, bin_educ_sup, bin_inac, horas_trabajadas)

#variables desde base personas a hogar
#estas variables son la suma, la tasa o el maximo de variables de interes

test_vars_personas_hogar <- test_personas |> 
  group_by(id) |>
  summarize(num_women    = sum(bin_woman, na.rm = TRUE),
            num_minors   = sum(bin_minor, na.rm = TRUE),
            num_mayores = sum(bin_mayor, na.rm = TRUE),
            num_dep = num_minors + num_mayores,
            cat_maxEduc  = max(cat_educ, na.rm = TRUE),
            num_educ_sup = sum(bin_educ_sup, na.rm = TRUE),
            num_c_propia = sum(bin_c_propia, na.rm = TRUE),
            num_h_trabajadas = sum(horas_trabajadas, na.rm = TRUE),
            num_ocu = sum(bin_occupied, na.rm = TRUE),
            num_pet = sum(pet, na.rm = TRUE),
            num_inac = sum(inac, na.rm=TRUE)) |> 
  mutate(t_ocu = num_ocu/num_pet,
         t_inac = num_inac/num_pet,
         t_educ_sup = num_educ_sup/num_pet,
         t_dependencia = if_else(num_pet > 0, 100 * num_dep / num_pet, NA_real_),
         t_c_propia = num_c_propia/num_pet) |>
  ungroup()
test_vars_personas_hogar <- test_vars_personas_hogar |> 
  select(-num_dep) #por multicolinealidad

#variables por jefe del hogar
test_jefe_hogar <- test_personas |> 
  filter(bin_head == 1) |>
  select(id, bin_woman, cat_educ, bin_social_sec, bin_occupied, edad, bin_c_propia,
         horas_trabajadas) |>
  rename(bin_headWoman = bin_woman,
         cat_educHead = cat_educ,
         edad_head = edad,
         bin_headSS = bin_social_sec,
         bin_headCpropia = bin_c_propia,
         h_workedHead = horas_trabajadas,
         bin_occupiedHead = bin_occupied) |>
  left_join(test_vars_personas_hogar, by = "id")

#variables de la base a nivel hogar
test_hogares <- test_hogares |>
  mutate(P5090 = ifelse(P5090==6,0,P5090)) |>
  rename(n_habitaciones = P5000,
         tipo_vivienda = P5090) |>
  select(id, Dominio, Nper, Lp, n_habitaciones, tipo_vivienda)

#base definitiva
test <- test_hogares |> 
  left_join(test_jefe_hogar) |>

#categóricas a factores
test <- test |> 
  mutate(Dominio = factor(Dominio),
         cat_maxEduc = factor(
           cat_maxEduc, 
           levels = c(0:6),
           labels=c("No sabe",'Ninguno', 'Preescolar', 'Primaria',
                    'Secundaria', 'Media', 'Universitaria')),
         cat_educHead = factor(
           cat_educHead, 
           levels = c(0:6),
           labels=c("No sabe",'Ninguno', 'Preescolar', 'Primaria',
                    'Secundaria', 'Media', 'Universitaria')),
         tipo_vivienda = factor(
           tipo_vivienda, 
           levels = c(0:5),
           labels=c("Otra", 'Propia, totalmente pagada','Propia, la están pagando',
                    'En arriendo o subarriendo', 'En usufructo',
                    'Posesión sin titulo')))

#guardar ambas bases
write.csv(train, "stores/train.csv", row.names = FALSE)
write.csv(test, "stores/test.csv", row.names = FALSE)