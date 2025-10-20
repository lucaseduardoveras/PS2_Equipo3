# PS2_Equipo3
Este es el repositorio para el segundo conjunto de porblemas del curso BDML.
Antes de usarlo es necesario que se descargue los 4 archivos :train_hogares.csv - training set with household-level information.
train_personas.csv - training set with individual-level information.
test_hogares.csv - test set with household-level information.
test_personas.csv - test set with individual-level information y guardarlos dentro la carpeta stores, una vez que la versión gratis de Github no nos permite trabajar con los archivo de este tamaño.
Todos los scripts que corremos fueron guardados en la carpeta script. Inicie la carpeta corriendo el código "data.R", este código crea las variables usada en la estimación y hace algunas manipulaciones. Al final este código crea train.csv y test.csv, que son los archivos usados en los modelos para predicir.
Cada modelos que corremos posee un script. Por ejemplo, model_cart.R hace las predicciones usando el modelo cart. Cada uno de estos scripts genera el archivo de predicciones, estes se quedan guardados en la carpeta "stores/modelos".
Dentro de la carpeta script también se encuentran los el script "data_descriptive.R" usado para generar tablas descriptivas sobre los datos de entrnamiento. Por fin, los archivos "roc_curves_for_CART.R" genera figuras de la curva ROC para algunos modelos. Estas figuras son guardadas en la carpeta "views".
Por fin la carpeta "document" contiene los archivos latex usados para generar el documento y la carpeta slides contienen los slides usados en las presentaciones.
