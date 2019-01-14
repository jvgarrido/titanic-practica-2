# titanic-practica-2
 Tipología y ciclo de vida de los datos
 
 Análisis en R sobre dataset del Titanic extraido de Kaggle
 Se ha procedido a la limpieza del dataset:
 seleccionando las variables relevantes
 tratando ceros y vacíos
 tratando valores extremos
 relizando agrupaciones de interés para el análisis
 Y está pendiente de realizar la comprobacion de la normalidad y la homogneización de la varianza
 Así como la aplicación de pruebas estadísticas la visualización y la conclusión
 El codigo en R se adjunta en este repositorio
 
Práctica 2. Tipología y Ciclo de vida de los datos
Jose Vicente Márquez Garrido

14 de enero de 2019

R Markdown
A continuación se muestra el código y el resultado de su ejecución que ha sido utilizado para la confección de la Practica 2 de Tipologia y ciclo de vida de los datos

1.- Descripción del Dataset

La colección de datos que es la fuente de este análisis procede de la web de Kaggle del siguiente enlace: https://www.kaggle.com/c/3136/download-all y se corresponde con un listado de pasajeros del Titanic tanto supervivientes como no.

Se trata de un dataset que consta de dos ficheros csv: train y test, cuyo nombre es autoexplicativo, puesto que se trata de una de las actividades iniciales de la página de Kaggle para entrenar nuestras habilidades en Data Science.

El fichero train contiene 891 registros(o filas) y 12 variables (o columnas). Las 12 variables con las siguientes:

PassengerId: un simple numero de identificador de pasajero.

Survived: Que nos indica si el pasajero sobrevivió con un 1 o sino con el valor 0

Pclass: Que nos indica en que clase viajaba el pasajero y que va de 1 hasta 3

Name: El nombre, apellidos y tratamiento del pasajeros

Sex: el sexo del pasajero

Age: la edad del pasajero

SibSp: el número de parientes hermanos o cónyuges abordo del Titanic

Parch: el número de parientes padres o hijos abordo de Titanic

Ticket: el identificador del ticket del pasajero

Fare: la tarifa pagada

Cabin: la cabina que ocupaban si es que tenían

Embarked: la ciudad donde embarcaron

2.- Integración y selección de los datos

# Lectura de datos: importación del dataset train.csv
titanic <- read.csv("train.csv")

#eliminamos las columnas 1, 4, y 9
titanic <- titanic[c(-1,-4,-9)]

#comprobamos que el dataframe tiene la estructura correcta listando las primeras filas
head(titanic[,1:9])
##   Survived Pclass    Sex Age SibSp Parch    Fare Cabin Embarked
## 1        0      3   male  22     1     0  7.2500              S
## 2        1      1 female  38     1     0 71.2833   C85        C
## 3        1      3 female  26     0     0  7.9250              S
## 4        1      1 female  35     1     0 53.1000  C123        S
## 5        0      3   male  35     0     0  8.0500              S
## 6        0      3   male  NA     0     0  8.4583              Q
#comprobamos que tipo de dato asigna R a las variables
sapply(titanic, function(x) class(x))
##  Survived    Pclass       Sex       Age     SibSp     Parch      Fare 
## "integer" "integer"  "factor" "numeric" "integer" "integer" "numeric" 
##     Cabin  Embarked 
##  "factor"  "factor"
3.- Limpieza de los datos

#Comprobamos si existen ceros en la columna Pclass
ceroPclass  <- subset(titanic,Pclass=="0")
ceroPclass
## [1] Survived Pclass   Sex      Age      SibSp    Parch    Fare     Cabin   
## [9] Embarked
## <0 rows> (or 0-length row.names)
#Comprobamos si existen ceros en la columna Age
ceroAge  <- subset(titanic,Age=="0")
ceroAge
## [1] Survived Pclass   Sex      Age      SibSp    Parch    Fare     Cabin   
## [9] Embarked
## <0 rows> (or 0-length row.names)
#comprobamos que columnas tienen valores vacíos y en que cantidad
sapply(titanic, function(x) sum(is.na(x)))
## Survived   Pclass      Sex      Age    SibSp    Parch     Fare    Cabin 
##        0        0        0      177        0        0        0        0 
## Embarked 
##        0
#primero obtenemos la media de edad por sexo
media_male_Age <- mean(titanic[titanic$Sex=="male", "Age"], na.rm=TRUE)
media_female_Age <- mean(titanic[titanic$Sex=="female", "Age"], na.rm=TRUE)

media_male_Age
## [1] 30.72664
media_female_Age
## [1] 27.91571
#sustituimos los valores vacios de Age por el valor medio de la edad de los pasajeros
titanic[is.na(titanic$Age) & titanic$Sex=="male","Age"] <- media_male_Age
titanic[is.na(titanic$Age) & titanic$Sex=="female","Age"] <- media_female_Age

#finalmente comprobamos que ya no existen valores vacíos en el dataframe
sapply(titanic, function(x) sum(is.na(x)))
## Survived   Pclass      Sex      Age    SibSp    Parch     Fare    Cabin 
##        0        0        0        0        0        0        0        0 
## Embarked 
##        0
#deteccion de valores extremos
boxplot.stats(titanic$Age)$out
##  [1]  2.00 58.00 55.00  2.00 66.00 65.00  0.83 59.00 71.00 70.50  2.00
## [12] 55.50  1.00 61.00  1.00 56.00  1.00 58.00  2.00 59.00 62.00 58.00
## [23] 63.00 65.00  2.00  0.92 61.00  2.00 60.00  1.00  1.00 64.00 65.00
## [34] 56.00  0.75  2.00 63.00 58.00 55.00 71.00  2.00 64.00 62.00 62.00
## [45] 60.00 61.00 57.00 80.00  2.00  0.75 56.00 58.00 70.00 60.00 60.00
## [56] 70.00  0.67 57.00  1.00  0.42  2.00  1.00 62.00  0.83 74.00 56.00
boxplot.stats(titanic$SibSp)$out
##  [1] 3 4 3 3 4 5 3 4 5 3 3 4 8 4 4 3 8 4 8 3 4 4 4 4 8 3 3 5 3 5 3 4 4 3 3
## [36] 5 4 3 4 8 4 3 4 8 4 8
boxplot.stats(titanic$Parch)$out
##   [1] 1 2 1 5 1 1 5 2 2 1 1 2 2 2 1 2 2 2 3 2 2 1 1 1 1 2 1 1 2 2 1 2 2 2 1
##  [36] 2 1 1 2 1 4 1 1 1 1 2 2 1 2 1 1 1 2 1 1 2 2 2 1 1 2 2 1 2 1 1 1 1 1 1
##  [71] 1 2 1 2 2 1 1 2 1 1 2 1 1 1 1 2 1 1 1 4 1 1 2 2 2 2 2 1 1 1 2 2 1 1 2
## [106] 2 3 4 1 2 1 1 2 1 2 1 2 1 1 2 2 1 1 1 1 2 2 2 2 2 2 1 1 2 1 4 1 1 2 1
## [141] 2 1 1 2 5 2 1 1 1 2 1 5 2 1 1 1 2 1 6 1 2 1 2 1 1 1 1 1 1 1 3 2 1 1 1
## [176] 1 2 1 2 3 1 2 1 2 2 1 1 2 1 2 1 2 1 1 1 2 1 1 2 1 2 1 1 1 1 3 2 1 1 1
## [211] 1 5 2
boxplot.stats(titanic$Fare)$out
##   [1]  71.2833 263.0000 146.5208  82.1708  76.7292  80.0000  83.4750
##   [8]  73.5000 263.0000  77.2875 247.5208  73.5000  77.2875  79.2000
##  [15]  66.6000  69.5500  69.5500 146.5208  69.5500 113.2750  76.2917
##  [22]  90.0000  83.4750  90.0000  79.2000  86.5000 512.3292  79.6500
##  [29] 153.4625 135.6333  77.9583  78.8500  91.0792 151.5500 247.5208
##  [36] 151.5500 110.8833 108.9000  83.1583 262.3750 164.8667 134.5000
##  [43]  69.5500 135.6333 153.4625 133.6500  66.6000 134.5000 263.0000
##  [50]  75.2500  69.3000 135.6333  82.1708 211.5000 227.5250  73.5000
##  [57] 120.0000 113.2750  90.0000 120.0000 263.0000  81.8583  89.1042
##  [64]  91.0792  90.0000  78.2667 151.5500  86.5000 108.9000  93.5000
##  [71] 221.7792 106.4250  71.0000 106.4250 110.8833 227.5250  79.6500
##  [78] 110.8833  79.6500  79.2000  78.2667 153.4625  77.9583  69.3000
##  [85]  76.7292  73.5000 113.2750 133.6500  73.5000 512.3292  76.7292
##  [92] 211.3375 110.8833 227.5250 151.5500 227.5250 211.3375 512.3292
##  [99]  78.8500 262.3750  71.0000  86.5000 120.0000  77.9583 211.3375
## [106]  79.2000  69.5500 120.0000  93.5000  80.0000  83.1583  69.5500
## [113]  89.1042 164.8667  69.5500  83.1583
4.- Análisis de Datos

#Grupo por sexo (Sex)
titanic.male <- titanic[titanic$Sex == "male",]
titanic.female <- titanic[titanic$Sex == "female",]

#Grupo por clase en la que viajan ( Pclass)
titanic.primera <- titanic[titanic$Pclass == "1",]
titanic.segunda <- titanic[titanic$Pclass == "2",]
titanic.tercera <- titanic[titanic$Pclass == "3",]

#Grupo por familiares abordo(SibSp y Parch)
titanic.sinfamilia <- titanic[titanic$SibSp=="0" & titanic$Parch=="0",]
titanic.confamilia <- titanic[titanic$SibSp != "0" | titanic$Parch !="0",]

#Grupo con cabina y sin cabina (Cabin)
titanic.concabina <- titanic[titanic$Cabin !="",]
titanic.sincabina <- titanic[titanic$Cabin =="",]
Including Plots
