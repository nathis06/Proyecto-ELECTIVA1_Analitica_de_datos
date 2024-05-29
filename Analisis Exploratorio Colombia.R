#Natalia Mogollon-201821207
#Julieth Plazas-201821049
#Sergio Ballen-201821143 


library(readr)
library(readxl)
library(tidyr)
library(dplyr)

#funcion mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#SUICIDIOS COLOMBIA
Suicidio_Colombia<-read_csv("C:/Users/Lenovo/Documents/SEMESTRE 8/ELECTIVA I/DATA SETS PROYECTO/Suicidios_Colombia.csv")
View(Suicidio_Colombia)

#NUMERO DE OBSERVACIONES
nrow(Suicidio_Colombia)

#NUMERO DE VARIABLES
ncol(Suicidio_Colombia)

#NUMERO DE VALORES FALTANTES
sum(is.na(Suicidio_Colombia))

#TIPO DE VARIABLES
summary(Suicidio_Colombia)
summary(Suicidio_Colombia$`Año del hecho`)

#HIPOTESIS
#2. EL MAYOR NUMERO DE CASOS DE INTENTO DE SUICIDIO EN LA CIUDAD DE TUNJA SE PRESENTA EN MUJERES
getmode(Suicidio_Colombia$`Sexo de la victima`)
#Conclusion: El resultado arroja que el sexo que mas intenta suicidarse o auto infringirse la muerte es el Hombre es decir el genro masculino, por lo que se rechaza la hipotesis planteada

