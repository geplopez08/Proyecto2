#Importación de los datasets

data_2023 <- read_excel("C:\\Users\\geplo\\OneDrive\\Escritorio\\MAESTRIA\\Ciclo 4\\Minería\\Proyecto 2\\Dataset\\2023\\data_2023.xlsx", sheet = "Basededatosdeganado")

data_2022 <- read_excel("C:\\Users\\geplo\\OneDrive\\Escritorio\\MAESTRIA\\Ciclo 4\\Minería\\Proyecto 2\\Dataset\\2022\\data_2022.xlsx", sheet = "paso6 baseParaUsuario")

#Hacer una expliracion rapida de los data sets

View(data_2022)
View(data_2023)

#Los dos datasets tienen 19 columnas con los mismos datos. El siguiente paso es unir las dos bases de datos para contar con un solo dataset para el analisis. Para ello es necesario eliminar la columna de correlativo; que corresponde únicamente al número de observación. Se eliminar ya que no aporta información relevante y puede dificultar la unión de los datasets.

data_2022 <- data_2022[, -c(1)]
data_2023 <- data_2023[,-c(1)]

#Al cargar los datos, algunas variables muestran las observaciones con numeros decimales; informacion que no forma parte de la fuente original. Por lo que es necesario transformar los valores de esas variables a numeros enteros.
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

#La variable "Peso vivo promedio (peso de cada cabeza)" muestra sus valores unicamente en dos decimales. Al momento de cargar el dataset de 2022, esta columna muestra valores con mas de cuatro decimales; y el dataset de 2023 con tres decimales. Por lo que es necesario hacer la transformacion de los datos.
data_2022$`Peso vivo promedio (peso de cada cabeza)`<- round(data_2022$`Peso vivo promedio (peso de cada cabeza)`, 2)
data_2023$`Peso vivo promedio (Peso de cada cabeza)`<- round(data_2023$`Peso vivo promedio (Peso de cada cabeza)`, 2)

#Antes de hacer la unión de los dos datasets es importante verificar que no hay datos nulos o vacíos.
anyNA(data_2022)
anyNA(data_2023)

#Al identificar los datos nulos, el siguiente paso es saber cuántos datos nulos hay en los datasets.
sum(is.na(data_2022))
sum(is.na(data_2023))

#Se identificó que los valores nulos corresponden a la columna "Peso total en libras". Sin embargo, estos valores corresponden al dato del tipo de carne de exportación, que no contiene información en esta variable, por lo que únicamente se llenarán los vacíos con ceros.
data_2022[is.na(data_2022)] <- 0
data_2023[is.na(data_2023)] <- 0

#A partir de esto, se procede a hacer la union de los dos datasets. Para ello es necesario instalar el paquete dplyr.
install.packages("dplyr")
library(dplyr)

#Para no tener problemas con la unión, es necesario que las columnas tengan el mismo nombre en los encabezados. Por lo que a continuación se procede a modificarlos.
View(data_2022)
View(data_2023)

colnames(data_2022)[colnames(data_2022) == "Número de cabezas"] <- "Número de Cabezas"
colnames(data_2022)[colnames(data_2022) == "Peso vivo promedio (peso de cada cabeza)"] <- "Peso vivo promedio (Peso de cada cabeza)"
colnames(data_2022)[colnames(data_2022) == "Carne y hueso"] <- "Carne y Hueso"

#A continuación se unen las tablas combinando las filas y se asigna la combinación a una nueva variable.
data_ganado <- bind_rows(data_2022, data_2023)

View(data_ganado)

##Minería de datos: Árboles de decisión
#Instalación de los paquetes y librerías
install.packages("rpart")
install.packages("rpart.plot")

library(rpart)
library(rpart.plot)

#Previamente se había identificado que el algoritmo no puede trabajar con encabezados con espacios, por lo que se modifican los nombres de las varibales categóricas
colnames(data_ganado)[colnames(data_ganado)== "Tipo de Carne"] <- "TipodeCarne"
colnames(data_ganado)[colnames(data_ganado)== "Sexo (subclase)"] <- "Sexo(subclase)"
colnames(data_ganado)[colnames(data_ganado)== "Sexo(subclase)"] <- "SexoSubclase"

#Desarrollo del árbol de decisión basado en departamento
arbol_departamento <- rpart(Departamento ~
                Clase+
                TipodeCarne+
                SexoSubclase,
               data = data_ganado, method = "class")

rpart.plot(arbol_departamento, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de departamento", cex = 1)

#Puesta a prueba del modelo de árbol de decisión departamento
predicción_departamento1  <- data.frame(
  Clase=c(1),
  TipodeCarne=c(2),
  SexoSubclase=c(5)
)

resultado_departamento1 <- predict(arbol_departamento,predicción_departamento1, type="class")
resultado_departamento1

predicción_departamento2  <- data.frame(
  Clase=c(2),
  TipodeCarne=c(1),
  SexoSubclase=c(8)
)

resultado_departamento2 <- predict(arbol_departamento,predicción_departamento2, type="class")
resultado_departamento2

predicción_departamento3  <- data.frame(
  Clase=c(4),
  TipodeCarne=c(1),
  SexoSubclase=c(8)
)

resultado_departamento3 <- predict(arbol_departamento,predicción_departamento3, type="class")
resultado_departamento3

predicción_departamento4  <- data.frame(
  Clase=c(3),
  TipodeCarne=c(1),
  SexoSubclase=c(9)
)

resultado_departamento4 <- predict(arbol_departamento,predicción_departamento4, type="class")
resultado_departamento4

predicción_departamento5  <- data.frame(
  Clase=c(1),
  TipodeCarne=c(1),
  SexoSubclase=c(5)
)

resultado_departamento5 <- predict(arbol_departamento,predicción_departamento5, type="class")
resultado_departamento5

##Desarrollo del árbol de decisión basado en municipio
#Factorizar variables
data_ganado$Municipio <- factor(data_ganado$Municipio)
data_ganado$Año <- factor(data_ganado$Año)
data_ganado$Mes <- factor(data_ganado$Mes)

#Generar un subset con los datos de 2022
subset_ganado_2022 <- subset(data_ganado, Año=="2022")
View(subset_ganado_2022)

#Creación del árbol de decisión
arbol_municipio <- rpart(Municipio ~
                          Clase+
                          SexoSubclase,
                          data = subset_ganado_2022, method = "class")

rpart.plot(arbol_municipio, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de municipio", cex = 1)

predicción_municipio1 <- data.frame(
  Clase=c(1),
  SexoSubclase=c(5)
)

resultado_municipio1 <- predict(arbol_municipio,predicción_municipio1, type="class")
resultado_municipio1

predicción_municipio2 <- data.frame(
  Clase=c(3),
  SexoSubclase=c(8)
)

resultado_municipio2 <- predict(arbol_municipio,predicción_municipio2, type="class")
resultado_municipio2
