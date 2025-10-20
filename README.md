# 🧠 PS2_Equipo3

Repositorio correspondiente al **Segundo Conjunto de Problemas (PS2)** del curso **Big Data y Machine Learning para Economía Aplicada (BDML)**.

---

## 📂 Estructura del repositorio

```
PS2_Equipo3/
├── scripts/            # Códigos fuente utilizados en la estimación y análisis
│   ├── data.R              # Limpieza y construcción de variables base (train.csv / test.csv)
│   ├── data_descriptive.R  # Generación de estadísticas descriptivas
│   ├── model_cart.R        # Modelo CART (árbol de decisión)
│   ├── ...                 # Otros modelos de predicción
│   └── roc_curves_for_CART.R  # Curvas ROC para modelos seleccionados
│
├── stores/
│   ├── train.csv           # Datos finales de entrenamiento (generados por data.R)
│   ├── test.csv            # Datos finales de prueba
│   ├── modelos/            # Archivos de predicciones de cada modelo
│   └── raw/                # (opcional) Datos originales descargados localmente
│
├── views/                  # Figuras y visualizaciones (e.g., curvas ROC)
│
├── document/               # Archivos LaTeX del informe final
│
└── slides/                 # Presentaciones utilizadas en clase
```

---

## 📦 Requisitos previos

Antes de ejecutar cualquier script, asegúrese de contar con los siguientes **archivos de datos** descargados manualmente (no incluidos en el repositorio por restricciones de tamaño de GitHub):

- `train_hogares.csv` — Datos de entrenamiento a nivel de hogar  
- `train_personas.csv` — Datos de entrenamiento a nivel individual  
- `test_hogares.csv` — Datos de prueba a nivel de hogar  
- `test_personas.csv` — Datos de prueba a nivel individual  

> 💡 **Importante:**  
> Guarde estos cuatro archivos dentro de la carpeta `stores/` antes de correr cualquier script.

---

## 🚀 Cómo ejecutar el proyecto

1. **Cree las bases de datos de modelado**  
   Ejecute el script principal:

   ```r
   source("scripts/data.R")
   ```

   Este código:
   - Limpia y combina los archivos de entrada  
   - Crea las variables utilizadas en la estimación  
   - Genera los archivos `train.csv` y `test.csv` usados en los modelos predictivos  

2. **Entrene los modelos de predicción**  
   Cada script de modelo (por ejemplo `model_cart.R`, `model_rf.R`, `model_elnet.R`) entrena un modelo distinto y genera su archivo de predicciones en:

   ```
   stores/modelos/
   ```

3. **Análisis descriptivo y visualizaciones**
   - `data_descriptive.R`: produce tablas descriptivas de los datos de entrenamiento.  
   - `roc_curves_for_CART.R`: genera figuras de las curvas ROC y las guarda en `views/`.

4. **Documentación**
   - Los archivos `.tex` en `document/` se usan para compilar el informe final.  
   - Las diapositivas de presentación están en `slides/`.

---

## 📊 Salidas principales

- **Predicciones**: `stores/modelos/*.csv`  
- **Curvas ROC y AUC**: `views/*.png`  
- **Informe académico (LaTeX)**: `document/*.tex`  
- **Presentaciones**: `slides/*.pdf`

---

## 🧩 Integrantes del equipo

- *Catalina Leal Rojas*  
- *Lucas Daniel Carrillo Aguirre*  
- *Lucas Eduardo Veras Costa*  
 
---

## 📝 Notas finales

- Este repositorio está diseñado para replicar el flujo completo de trabajo del PS2.  
- Dado el tamaño de los archivos originales, **GitHub no almacena los datasets brutos**.  
- Todos los scripts pueden ejecutarse en **R ≥ 4.3** con las dependencias listadas en cada archivo.

---
