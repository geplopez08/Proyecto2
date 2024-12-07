"""
Script: Proyecto parte 2
Descripción: Este script presenta el proceso de limpieza de datos y ejecución de un código para generar árboles de decisión, bosques aleatorios, y redes neuronales.
Autor: Giovanni Emanuel Pacheco López
Fecha: 2024-12-07
"""
#Proyecto parte 2: Minería de datos

## Tabla de Contenidos
1. [Introducción](##Introducción)
2. [Requisitos](##Requisitos)
3. [Procedimiento](##Procedimiento)

##Introducción
Este proyecto incluye el código para la generación de árboles de decisión, bosques aleatorios y redes neuronales para la predicción sobre datos de destace de ganado en Guatemala de los años 2022 y 2023.

##Requisitos
- R 4.0 o superior
    - Paquetes:"readxl", "dplyr", "rpart", "rpart.plot", "randomForest", "dplyr"
    - Librerías: "readxl", "dplyr", "rpart", "rpart.plot", "randomForest", "dplyr"
- Python (entorno de Google Colab)
    - Librerías: numpy, pandas, tensorflor
- Sistema operativo: Windows 11

##Procedimiento

#Importación de los datasets
Estos datasets provienen del repositorio del Instituto Nacional de Estadística (INE), ubicado en el siguiente enlace: (https://www.ine.gob.gt/estadisticas-agropecuarias/)
Se descargan los dataset del año 2022 y 2023 en formato xls, y se importan a R.

```r
data_2023 <- read_excel("C:ruta en donde se encuentra el archivo", sheet = "Nombre de la hoja del archivo de excel")

data_2022 <- read_excel("C:ruta en donde se encuentra el archivo", sheet = "Nombre de la hoja del archivo de excel")
```
##Limpieza de datos

#Se realiza una exploración de los datos para conocerlos.
```r
View(data_2022)
View(data_2023)
```
#Los dos datasets tienen 19 columnas con los mismos datos. El siguiente paso es unir las dos datasets para contar con un solo dataset para el analisis. Para ello es necesario eliminar la columna de correlativo; que corresponde únicamente al número de observación. Se eliminar ya que no aporta información relevante y puede dificultar la unión de los datasets.
```r
data_2022 <- data_2022[, -c(1)]
data_2023 <- data_2023[,-c(1)]
```
#Al cargar los datos, algunas variables muestran las observaciones con numeros decimales; informacion que no forma parte de la fuente original. Por lo que es necesario transformar los valores de esas variables a numeros enteros.
```r
data_2022$`Peso total en libras`<- round(data_2022$`Peso total en libras`)
data_2022$`Peso total del número de cabezas (quintales)`<- round(data_2022$`Peso total del número de cabezas (quintales)`)
data_2022$`Carne y hueso` <- round(data_2022$`Carne y hueso`)
data_2022$Sebo <- round(data_2022$Sebo)
data_2022$Total <- round(data_2022$Total)
data_2022$Vísceras <- round(data_2022$Vísceras)
data_2022$Cuero <- round(data_2022$Cuero)
data_2022$Sangre <- round(data_2022$Sangre)
data_2022$Desperdicio <- round(data_2022$Desperdicio)

data_2023$`Peso total en libras` <- round(data_2023$`Peso total en libras`)
data_2023$`Peso total del número de cabezas (quintales)`<- round(data_2023$`Peso total del número de cabezas (quintales)`)
data_2023$`Carne y Hueso`<- round(data_2023$`Carne y Hueso`)
data_2023$Sebo <- round(data_2023$Sebo)
data_2023$Total <- round(data_2023$Total)
data_2023$Vísceras <- round(data_2023$Vísceras)
data_2023$Cuero <- round(data_2023$Cuero)
data_2023$Sangre <- round(data_2023$Sangre)
data_2023$Desperdicio <- round(data_2023$Desperdicio)
```
#La variable "Peso vivo promedio (peso de cada cabeza)" muestra sus valores unicamente en dos decimales. Al momento de cargar el dataset de 2022, esta columna muestra valores con mas de cuatro decimales; y el dataset de 2023 con tres decimales. Por lo que es necesario hacer la transformacion de los datos.
```r
data_2022$`Peso vivo promedio (peso de cada cabeza)`<- round(data_2022$`Peso vivo promedio (peso de cada cabeza)`, 2)
data_2023$`Peso vivo promedio (Peso de cada cabeza)`<- round(data_2023$`Peso vivo promedio (Peso de cada cabeza)`, 2)
```
#Antes de hacer la unión de los dos datasets es importante verificar que no hay datos nulos o vacíos.
```r
anyNA(data_2022)
anyNA(data_2023)
```
#Al identificar los datos nulos, el siguiente paso es saber cuántos datos nulos hay en los datasets.
```r
sum(is.na(data_2022))
sum(is.na(data_2023))
```
#Se identificó que los valores nulos corresponden a la columna "Peso total en libras". Sin embargo, estos valores corresponden al dato del tipo de carne de exportación, que no contiene información en esta variable, por lo que únicamente se llenarán los vacíos con ceros.
```r
data_2022[is.na(data_2022)] <- 0
data_2023[is.na(data_2023)] <- 0
```
#A partir de esto, se procede a hacer la union de los dos datasets. Para ello es necesario instalar el paquete dplyr.
```r
install.packages("dplyr")
library(dplyr)
```
#Para no tener problemas con la unión, es necesario que las columnas tengan el mismo nombre en los encabezados. Por lo que a continuación se procede a modificarlos.
```r
View(data_2022)
View(data_2023)

colnames(data_2022)[colnames(data_2022) == "Número de cabezas"] <- "Número de Cabezas"
colnames(data_2022)[colnames(data_2022) == "Peso vivo promedio (peso de cada cabeza)"] <- "Peso vivo promedio (Peso de cada cabeza)"
colnames(data_2022)[colnames(data_2022) == "Carne y hueso"] <- "Carne y Hueso"
```
#A continuación se unen las tablas combinando las filas y se asigna la combinación a una nueva variable.
```r
data_ganado <- bind_rows(data_2022, data_2023)

View(data_ganado)
```
##Minería de datos: Árboles de decisión
En este apartado se desarrollan cuatro árboles de decisión y ejercicios para predicción de casos

#Para generar árboles es necesario la instalación de los siguientes paquetes y librerías
```r
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
```
#Previamente se había identificado que el algoritmo no puede trabajar con encabezados con espacios, por lo que se modifican los nombres de las varibales categóricas
```r
colnames(data_ganado)[colnames(data_ganado)== "Tipo de Carne"] <- "TipodeCarne"
colnames(data_ganado)[colnames(data_ganado)== "Sexo (subclase)"] <- "Sexo(subclase)"
colnames(data_ganado)[colnames(data_ganado)== "Sexo(subclase)"] <- "SexoSubclase"
```
###Desarrollo del árbol de decisión basado en departamento
En este ejercicio se desarrolla un árbol de decisión para la predicción de departamento en función de Clase, Tipo de Carne y Sexo (subclase).
```r
arbol_departamento <- rpart(Departamento ~
                Clase+
                TipodeCarne+
                SexoSubclase,
               data = data_ganado, method = "class")

rpart.plot(arbol_departamento, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de departamento", cex = 1)
```
#Puesta a prueba del modelo de árbol de decisión departamento
En este caso se pone a prueba el modelo para predecir el departamento por la clase bovino, tipo de carne de exportación, y Sexo (sublclase) vaca.
```r
predicción_departamento1  <- data.frame(
  Clase=c(1),
  TipodeCarne=c(2),
  SexoSubclase=c(5)
)

resultado_departamento1 <- predict(arbol_departamento,predicción_departamento1, type="class")
resultado_departamento1
```

En este caso se pone a prueba el modelo para predecir el departamento por la clase porcino, tipo de carne de consumo interno, y Sexo (sublclase) macho.
```r
predicción_departamento2  <- data.frame(
  Clase=c(2),
  TipodeCarne=c(1),
  SexoSubclase=c(8)
)

resultado_departamento2 <- predict(arbol_departamento,predicción_departamento2, type="class")
resultado_departamento2
```

En este caso se pone a prueba el modelo para predecir el departamento por la clase caprino, tipo de carne de consumo interno, y Sexo (sublclase) macho.
```r
predicción_departamento3  <- data.frame(
  Clase=c(4),
  TipodeCarne=c(1),
  SexoSubclase=c(8)
)

resultado_departamento3 <- predict(arbol_departamento,predicción_departamento3, type="class")
resultado_departamento3
```
En este caso se pone a prueba el modelo para predecir el departamento por la clase ovino, tipo de carne de consumo interno, y Sexo (sublclase) hembra.
```r
predicción_departamento4  <- data.frame(
  Clase=c(3),
  TipodeCarne=c(1),
  SexoSubclase=c(9)
)

resultado_departamento4 <- predict(arbol_departamento,predicción_departamento4, type="class")
resultado_departamento4
```
En este caso se pone a prueba el modelo para predecir el departamento por la clase bovino, tipo de carne de consumo interno, y Sexo (sublclase) vaca.
```r
predicción_departamento5  <- data.frame(
  Clase=c(1),
  TipodeCarne=c(1),
  SexoSubclase=c(5)
)

resultado_departamento5 <- predict(arbol_departamento,predicción_departamento5, type="class")
resultado_departamento5
```

###Desarrollo del árbol de decisión basado en municipio
En este ejercicio se desarrolla un árbol de decisión para la predicción de municipio en función de Clase, y Sexo (subclase).

#Factorizar variables
Para este caso se asegura que la variable municipio esté factorizada

```r
data_ganado$Municipio <- factor(data_ganado$Municipio)
```

#Generar un subset con los datos de 2022
Previamente se había identificado valores incorrectos en la variable de municipio del dataset de 2023. Por lo que se genera un subset con los datos de 2022, y trabajar la predicción.

```r
subset_ganado_2022 <- subset(data_ganado, Año=="2022")
View(subset_ganado_2022)
```

#Creación del árbol de decisión basado en municipio
```r
arbol_municipio <- rpart(Municipio ~
                          Clase+
                          SexoSubclase,
                          data = subset_ganado_2022, method = "class")


rpart.plot(arbol_municipio, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de municipio", cex = 1)
```
#Creación del árbol de decisión forzando a 3 niveles y minsplit de 5
El modelo anterior no generó ramas, por lo que se forza la generación de 3 ramas a partir de 5 observaciones mínimas

```r
arbol_municipio <- rpart(Municipio ~
                           Clase+
                           SexoSubclase,
                         data = subset_ganado_2022, 
               control = rpart.control(minsplit = 5, cp = 0, maxdepth = 3))

rpart.plot(arbol_municipio, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de municipio", cex = 1)
```

#Puesta a prueba del modelo de árbol de decisión municipio
En este caso se pone a prueba el modelo para predecir el municipio por la clase bovino, y Sexo (sublclase) vaca.
```r
predicción_municipio1 <- data.frame(
  Clase=c(1),
  SexoSubclase=c(5)
)

resultado_municipio1 <- predict(arbol_municipio,predicción_municipio1, type="class")
resultado_municipio1
```

En este caso se pone a prueba el modelo para predecir el municipio por la clase bovino, y Sexo (sublclase) vaca.
```r
predicción_municipio2 <- data.frame(
  Clase=c(3),
  SexoSubclase=c(8)
)

resultado_municipio2 <- predict(arbol_municipio,predicción_municipio2, type="class")
resultado_municipio2
```

En este caso se pone a prueba el modelo para predecir el municipio por la clase bovino, y Sexo (sublclase) vaca.
```r
predicción_municipio3 <- data.frame(
  Clase=c(1),
  SexoSubclase=c(5)
)

resultado_municipio3 <- predict(arbol_municipio,predicción_municipio3, type="class")
resultado_municipio3
```

En este caso se pone a prueba el modelo para predecir el municipio por la clase caprino, y Sexo (sublclase) hembra.
```r
predicción_municipio4 <- data.frame(
  Clase=c(4),
  SexoSubclase=c(9)
)

resultado_municipio4 <- predict(arbol_municipio,predicción_municipio4, type="class")
resultado_municipio4
```

##Desarrollo del árbol de decisión basado en Clase de carne
En este ejercicio se desarrolla un árbol de decisión para la predicción de Clase de carne en función de Tipo de Carne, Departamento, y Sexo (subclase).

```r
arbol_ClaseCarne <- rpart(Clase ~
                          TipodeCarne+
                          Departamento+
                          SexoSubclase,
                         data = data_ganado, method = "class")

rpart.plot(arbol_ClaseCarne, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de Clase de carne", cex = 1)
```

En este caso se pone a prueba el modelo para predecir el la clase de carne por el tipo de carne de consumo interno, el departamento de Suchitepequez y Sexo (sublclase) vaca.
```r
predicción_ClaseCarne1 <- data.frame(
  TipodeCarne=c(1),
  Departamento=c(10),
  SexoSubclase=c(5)
)

resultado_ClaseCarne1 <- predict(arbol_ClaseCarne,predicción_ClaseCarne1, type="class")
resultado_ClaseCarne1
```

En este caso se pone a prueba el modelo para predecir el la clase de carne por el tipo de carne de exportación, el departamento de Huehuetenango y Sexo (sublclase) ternero.
```r
predicción_ClaseCarne2 <- data.frame(
  TipodeCarne=c(2),
  Departamento=c(13),
  SexoSubclase=c(4)
)

resultado_ClaseCarne2 <- predict(arbol_ClaseCarne,predicción_ClaseCarne2, type="class")
resultado_ClaseCarne2
```

En este caso se pone a prueba el modelo para predecir el la clase de carne por el tipo de carne de exportación, el departamento de Sololá y Sexo (sublclase) hembra.
```r
predicción_ClaseCarne3 <- data.frame(
  TipodeCarne=c(1),
  Departamento=c(7),
  SexoSubclase=c(9)
)

resultado_ClaseCarne3 <- predict(arbol_ClaseCarne,predicción_ClaseCarne3, type="class")
resultado_ClaseCarne3
```

##Desarrollo del árbol de decisión basado en mes
En este ejercicio se desarrolla un árbol de decisión para la predicción de Mes en función de Tipo de Carne, y Departamento.

```r
arbol_Mes <- rpart(Mes ~
                  TipodeCarne+
                  Departamento,
                  data = data_ganado, method = "class")

rpart.plot(arbol_Mes, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de Mes", cex = 1)
```

#Creación del árbol de decisión forzando a 3 niveles y minsplit de 5
El modelo anterior no generó ramas, por lo que se forza la generación de 3 ramas a partir de 5 observaciones mínimas

```r
arbol_Mes <- rpart(Mes ~
                 TipodeCarne+
                 Departamento,
               data = data_ganado, 
               control = rpart.control(minsplit = 5, cp = 0, maxdepth = 3))

rpart.plot(arbol_Mes, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de Mes", cex = 1)
```

#Puesta a prueba del modelo de árbol de decisión Mes
En este caso se pone a prueba el modelo para predecir el mes por tipo de carne de consumo interno, y el departamento de Guatemala.
```r
predicción_Mes1 <- data.frame(
  TipodeCarne=c(1),
  Departamento=c(1)
)

resultado_Mes1 <- predict(arbol_Mes,predicción_Mes1, type="class")
resultado_Mes1
```

En este caso se pone a prueba el modelo para predecir el mes por tipo de carne de exportación, y el departamento de Zacapa.
```r
predicción_Mes2 <- data.frame(
  TipodeCarne=c(2),
  Departamento=c(19)
)

resultado_Mes2 <- predict(arbol_Mes,predicción_Mes2, type="class")
resultado_Mes2
```

En este caso se pone a prueba el modelo para predecir el mes por tipo de carne de consumo interno, y el departamento de Quetzaltenango.
```r
predicción_Mes3 <- data.frame(
  TipodeCarne=c(1),
  Departamento=c(9)
)

resultado_Mes3 <- predict(arbol_Mes,predicción_Mes3, type="class")
resultado_Mes3
```

##Aplicación de bosques aleatorios (Random forests)
En este ejercicio se generan dos bosques aleatorios. Los modelo generados se utilizan para comparar los resultados de los árboles de decisión.

#Instalación de paquetes y librerias
Para la generación de los árboles se requiere la instalación del paquete y librería siguiente:

```r
install.packages("randomForest")
library(randomForest)
```

#Generación de un dataframe con columnas con valores categóricos
```r
subset_bosque <- data_ganado[, c("TipodeCarne","Mes","Departamento","Clase","SexoSubclase")]
View(subset_bosque)
```
#Factorización de la variable a predecir
```r
subset_bosque$Departamento <- factor(subset_bosque$Departamento)
```

#Generación de semilla y aleatoriedad de los datos
```r
set.seed(100)
subset_bosque <- subset_bosque[sample(1:nrow(subset_bosque)),]

index <-sample(1:nrow(subset_bosque), 0.8*nrow(subset_bosque))

train <- subset_bosque[index,]
test <- subset_bosque[-index,]
```

#Generación del bosque aleatorio en función de departamento
En este ejercicio se genera el bosque aleatorio para predecir Departamento en función de Clase, Tipo de Carne, y Sexo (subclase).

```r
bosque_departamento <- randomForest(Departamento ~ 
                        Clase+
                        TipodeCarne+
                        SexoSubclase,
                       data = train,
                       ntree = 1000
)

prediccion_bosque_departamento <- predict(bosque_departamento, test)
prediccion_bosque_departamento
```

#Visualización del modelo a partir de una matriz de confusión
```r
matriz <- table(test$Departamento, prediccion_bosque_departamento)
matriz
```

#Gráfico del bosque aleatorio de departamento
```r
plot(bosque_departamento)
```

#Puesta a prueba del modelo de bosque aleatorio de departamento
En este caso se pone a prueba el modelo para predecir el departamento por clase de carne porcino, tipo de carne de consumo interno, y sexo (subclase) macho.

```r
predict_departamento1 <- data.frame(
  Clase=2,
  TipodeCarne=1,
  SexoSubclase=8
)

predict_bosque_departamento1 <- predict(bosque_departamento, predict_departamento1)
predict_bosque_departamento1
```

En este caso se pone a prueba el modelo para predecir el departamento por clase de carne bovino, tipo de carne de consumo interno, y sexo (subclase) novilla.
```r
predict_departamento2 <- data.frame(
  Clase=1,
  TipodeCarne=1,
  SexoSubclase=6
)

predict_bosque_departamento2 <- predict(bosque_departamento, predict_departamento2)
predict_bosque_departamento2
```

En este caso se pone a prueba el modelo para predecir el departamento por clase de carne ovino, tipo de carne de consumo interno, y sexo (subclase) hembra.
```r
predict_departamento3 <- data.frame(
  Clase=3,
  TipodeCarne=1,
  SexoSubclase=9
)

predict_bosque_departamento3 <- predict(bosque_departamento, predict_departamento3)
predict_bosque_departamento3
```

En este caso se pone a prueba el modelo para predecir el departamento por clase de carne porcino, tipo de carne de consumo interno, y sexo (subclase) macho.
```r
predict_departamento4 <- data.frame(
  Clase=4,
  TipodeCarne=1,
  SexoSubclase=8
)

predict_bosque_departamento4 <- predict(bosque_departamento, predict_departamento4)
predict_bosque_departamento4
```

En este caso se pone a prueba el modelo para predecir el departamento por clase de carne bovino, tipo de carne de exportación, y sexo (subclase) vaca.
```r
predict_departamento5 <- data.frame(
  Clase=1,
  TipodeCarne=2,
  SexoSubclase=5
)

predict_bosque_departamento5 <- predict(bosque_departamento, predict_departamento5)
predict_bosque_departamento5
```

En este caso se pone a prueba el modelo para predecir el departamento por clase de carne porcino, tipo de carne de exportación, y sexo (subclase) hembra.
```r
predict_departamento6 <- data.frame(
  Clase=2,
  TipodeCarne=2,
  SexoSubclase=9
)

predict_bosque_departamento6 <- predict(bosque_departamento, predict_departamento6)
predict_bosque_departamento6
```

En este caso se pone a prueba el modelo para predecir el departamento por clase de carne ovino, tipo de carne de exportación, y sexo (subclase) hembra.
```r
predict_departamento7 <- data.frame(
  Clase=3,
  TipodeCarne=2,
  SexoSubclase=9
)

predict_bosque_departamento7 <- predict(bosque_departamento, predict_departamento7)
predict_bosque_departamento7
```

En este caso se pone a prueba el modelo para predecir el departamento por clase de carne caprino, tipo de carne de exportación, y sexo (subclase) macho.
```r
predict_departamento8 <- data.frame(
  Clase=4,
  TipodeCarne=2,
  SexoSubclase=8
)

predict_bosque_departamento8 <- predict(bosque_departamento, predict_departamento8)
predict_bosque_departamento8
```

##Bosque aleatorio en función de Clase de carne
En este ejercicio se genera el bosque aleatorio para predecir Clase de carne en función de Departamento y Tipo de Carne.


#Generación de un dataframe con columnas con valores categóricos
```r
subset_bosque <- data_ganado[, c("TipodeCarne","Mes","Departamento","Clase","SexoSubclase")]
```

#Factorización de variables
```r
subset_bosque$Clase <- factor(subset_bosque$Clase)
```

#Generación de semilla y aleatoriedad de los datos
```r
set.seed(100)
subset_bosque <- subset_bosque[sample(1:nrow(subset_bosque)),]

index <-sample(1:nrow(subset_bosque), 0.8*nrow(subset_bosque))

train <- subset_bosque[index,]
test <- subset_bosque[-index,]
```

#Generación del bosque aleatorio en función de Clase
```r
bosque_Clasecarne <- randomForest(Clase ~ 
                                  Departamento+
                                  TipodeCarne,
                                  data = train,
                                  ntree = 100
)

prediccion_clasecarne<- predict(bosque_Clasecarne, test)
prediccion_clasecarne
```

#Visualización del modelo a partir de una matriz de confusión
```r
matriz <- table(test$Clase, prediccion_clasecarne)
matriz
```

#Gráfico del bosque aleatorio de departamento
```r
plot(bosque_Clasecarne)
```

#Puesta a prueba del modelo de bosque aleatorio de Clase de carne
En este caso se pone a prueba el modelo para predecir de clase de carne por el departamento de Suchitepéquez y tipo de carne de consumo interno.

```r
predict_clasecarne1 <- data.frame(
  Departamento=10,
  TipodeCarne=1
)

predict_bosque_clasecarne1 <- predict(bosque_Clasecarne, predict_clasecarne1)
predict_bosque_clasecarne1
```

En este caso se pone a prueba el modelo para predecir de clase de carne por el departamento de Huehuetenango y tipo de carne de exportación.
```r
predict_clasecarne2 <- data.frame(
  Departamento=13,
  TipodeCarne=2
)

predict_bosque_clasecarne2 <- predict(bosque_Clasecarne, predict_clasecarne2)
predict_bosque_clasecarne2
```

En este caso se pone a prueba el modelo para predecir de clase de carne por el departamento de Sololá y tipo de carne de consumo interno.
```r
predict_clasecarne3 <- data.frame(
  Departamento=7,
  TipodeCarne=1
)

predict_bosque_clasecarne3 <- predict(bosque_Clasecarne, predict_clasecarne3)
predict_bosque_clasecarne3
```

En este caso se pone a prueba el modelo para predecir de clase de carne por el departamento de Jutiapa y tipo de carne de exportación.
```r
predict_clasecarne4 <- data.frame(
  Departamento=22,
  TipodeCarne=2
)

predict_bosque_clasecarne4 <- predict(bosque_Clasecarne, predict_clasecarne4)
predict_bosque_clasecarne4
```

##Generación de red neuronal para predicción: creación de csv a partir de los cuales se generaron los árboles de decisión
Las redes neuronales se desarrollan en el entorno de Google Colab, para ello se utiliza el dataset limpio y transformado. Para ello se genera un archivo csv de data_ganado.

#Establecer directorio y guardar el csv
```r
setwd("C:ruta al directorio en donde se desea guardar el archivo")
write.csv(data_ganado, "red_neuronal_data_ganado.csv", row.names = FALSE)
```