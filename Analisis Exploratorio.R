install.packages("ggplot2")

library(readr)
library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(nortest)
Tunja2<-read_excel("C:/Users/Lenovo/Documents/SEMESTRE 8/ELECTIVA I/DATA SETS PROYECTO/intentos_suicidio_tunja_2.xlsx")
View(Tunja2)


#Resumen estadistico
summary(Tunja2)
summary(Tunja2$ID_ANO)
ncol(Tunja2)
nrow(Tunja2)
sum(is.na(Tunja2))

#funcion mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Arreglo variable genero 
Tunja2$SEXO<-gsub("F","FEMENINO",Tunja2$SEXO)
Tunja2$SEXO<-gsub("FEMENINOEMENINO","FEMENINO",Tunja2$SEXO)

#Arreglo variable edad
class(Tunja2$EDAD_DE_LA_VICTIMA)
Tunja2$EDAD_DE_LA_VICTIMA<-gsub("años","",Tunja2$EDAD_DE_LA_VICTIMA)
Tunja2$EDAD_DE_LA_VICTIMA<-as.numeric(Tunja2$EDAD_DE_LA_VICTIMA)
Tunja2[is.na(Tunja2$EDAD_DE_LA_VICTIMA),"EDAD_DE_LA_VICTIMA"]<-mean(Tunja2$EDAD_DE_LA_VICTIMA, na.rm = T)
Tunja2$EDAD_DE_LA_VICTIMA<-round(Tunja2$EDAD_DE_LA_VICTIMA)

#Arreglo variable estrato
Tunja2$ESTRATO_SOCIOECONOMICO<-gsub("ESTRATO 3","3",Tunja2$ESTRATO_SOCIOECONOMICO)
Tunja2$ESTRATO_SOCIOECONOMICO<-gsub("ESTRATO 2","2",Tunja2$ESTRATO_SOCIOECONOMICO)
Tunja2$ESTRATO_SOCIOECONOMICO<-as.numeric(Tunja2$ESTRATO_SOCIOECONOMICO)
Tunja2$ESTRATO_SOCIOECONOMICO<-round(Tunja2$ESTRATO_SOCIOECONOMICO)


#Arreglo variable Gestante
Tunja2$GESTANTE<-gsub("NO APLICA","NO",Tunja2$GESTANTE)


#Arreglo variable Intentos previos
Tunja2$`INTENTOS_ PREVIOS`<-gsub("SII","SI",Tunja2$`INTENTOS_ PREVIOS`)
Tunja2$`INTENTOS_ PREVIOS`<-gsub("NOO","NO",Tunja2$`INTENTOS_ PREVIOS`)
Tunja2$`INTENTOS_ PREVIOS`<-gsub("S","SI",Tunja2$`INTENTOS_ PREVIOS`)
Tunja2$`INTENTOS_ PREVIOS`<-gsub("N","NO",Tunja2$`INTENTOS_ PREVIOS`)

#Arreglo escolaridad
Tunja2$ESCOLARIDAD<-gsub("PROF","PROFESIONAL",Tunja2$ESCOLARIDAD)
Tunja2$ESCOLARIDAD<-gsub("B - SECUNDARIA","BASICA SECUNDARIA",Tunja2$ESCOLARIDAD)

#Arreglo Estado civil
Tunja2$ESTADO_CIVIL<-gsub("U -LIBRE","UNION LIBRE",Tunja2$ESTADO_CIVIL)

#Arreglo problemas economicos
Tunja2$`PROBLEMAS_ ECONOMICOS`<-gsub("NOO","NO",Tunja2$`PROBLEMAS_ ECONOMICOS`)
Tunja2$`PROBLEMAS_ ECONOMICOS`<-gsub("N","NO",Tunja2$`PROBLEMAS_ ECONOMICOS`)

#Arreglo Area residencia
sum(is.na(Tunja2$`AREA_ DE_ RESIDENCIA`))
Tunja2[is.na(Tunja2$`AREA_ DE_ RESIDENCIA`),"AREA_ DE_ RESIDENCIA"]<-getmode(Tunja2$`AREA_ DE_ RESIDENCIA`)

#Arreglo Barrio de residencia
filter(Tunja2, grepl('4',Tunja2$`BARRIO_ DE_ RESIDENCIA`))
Tunja2=Tunja2[!(Tunja2$`BARRIO_ DE_ RESIDENCIA`=="44696.0"),]
Tunja2=Tunja2[!(Tunja2$`BARRIO_ DE_ RESIDENCIA`=="44762.0"),]

#Arreglo fecha
Tunja2=rename(Tunja2,FECHA_DE_NOTIFICACION_DEL_EVENTO='FECHA_DE_ NOTIFICACION_ DEL_ EVENTO')
Tunja2$FECHA_DE_NOTIFICACION_DEL_EVENTO <- as.Date(Tunja2$FECHA_DE_NOTIFICACION_DEL_EVENTO,format("%m/%d/%y"))
Tunja2$`FECHA_ DEL_ HECHO` <- as.Date(Tunja2$`FECHA_ DEL_ HECHO`,format("%m/%d/%y"))

#Arreglo variable lanzamiento de agua
Tunja2$`LANZAMIENTO_ AL_ AGUA`<-gsub("FALSE","NO",Tunja2$`LANZAMIENTO_ AL_ AGUA`)


#INTENTOS SUICIDIO TUNJA
Suicidio_Tunja<-read_excel("C:/Users/Lenovo/Documents/SEMESTRE 8/ELECTIVA I/DATA SETS PROYECTO/intentos_suicidio_tunja_1.xlsx")
View(Suicidio_Tunja)

#NUMERO OBSERVACIONES DEL DATASET
nrow(Suicidio_Tunja)

#NUMERO VARIABLES 
ncol(Suicidio_Tunja)
dim(Suicidio_Tunja)

#NUMERO VARIABLES FALTANTES

sum(is.na(Suicidio_Tunja))

#TIPO VARIABLES
summary(Suicidio_Tunja)


#RESUMEN VARIABLES NUMERICAS
summary(Suicidio_Tunja$ID_ANO)
summary(Suicidio_Tunja$`NUMERO_ DE_ INTENTOS`)

#MODA VARIABLES
getmode(Suicidio_Tunja$SEXO)
getmode(Suicidio_Tunja$ESTRATO_SOCIOECONOMICO)

#Arreglo Variable Genero 
Suicidio_Tunja$SEXO<-gsub("mujer","FEMENINO", Suicidio_Tunja$SEXO)
Suicidio_Tunja$SEXO<-gsub("hombre","MASCULINO", Suicidio_Tunja$SEXO)

#Arreglo Variables Edad
class(Suicidio_Tunja$EDAD_DE_LA_VICTIMA)
Suicidio_Tunja$EDAD_DE_LA_VICTIMA<-gsub("años","",Suicidio_Tunja$EDAD_DE_LA_VICTIMA)
Suicidio_Tunja$EDAD_DE_LA_VICTIMA<-as.numeric(Suicidio_Tunja$EDAD_DE_LA_VICTIMA)
Suicidio_Tunja[is.na(Suicidio_Tunja$EDAD_DE_LA_VICTIMA),"EDAD_DE_LA_VICTIMA"]<-mean(Suicidio_Tunja$`EDAD_DE_LA_VICTIMA`, na.rm=T)
Suicidio_Tunja$EDAD_DE_LA_VICTIMA<-round(Suicidio_Tunja$EDAD_DE_LA_VICTIMA)

#Arreglo Variable Unidad de Medida de la victima
Suicidio_Tunja$`UNIDAD_DE_MEDIDA_DE _LA_VICTIMA`<-gsub("ag","AÑOS",Suicidio_Tunja$`UNIDAD_DE_MEDIDA_DE _LA_VICTIMA`)
Suicidio_Tunja[is.na(Suicidio_Tunja$`UNIDAD_DE_MEDIDA_DE _LA_VICTIMA`),"UNIDAD_DE_MEDIDA_DE _LA_VICTIMA"]<-getmode(Suicidio_Tunja$`UNIDAD_DE_MEDIDA_DE _LA_VICTIMA`)


#Arreglo variables Estrato
Suicidio_Tunja$ESTRATO_SOCIOECONOMICO<-gsub("dos","2", Suicidio_Tunja$ESTRATO_SOCIOECONOMICO)
Suicidio_Tunja$ESTRATO_SOCIOECONOMICO<-gsub("tres","3.0", Suicidio_Tunja$ESTRATO_SOCIOECONOMICO)
Suicidio_Tunja$ESTRATO_SOCIOECONOMICO<-gsub("cuatro","4.0", Suicidio_Tunja$ESTRATO_SOCIOECONOMICO)
Suicidio_Tunja$ESTRATO_SOCIOECONOMICO<-gsub("uno","1.0", Suicidio_Tunja$ESTRATO_SOCIOECONOMICO)
Suicidio_Tunja$ESTRATO_SOCIOECONOMICO<-as.numeric(Suicidio_Tunja$ESTRATO_SOCIOECONOMICO)
Suicidio_Tunja$ESTRATO_SOCIOECONOMICO<-round(Suicidio_Tunja$ESTRATO_SOCIOECONOMICO)


#Arreglo variables fecha
?rename
class(Suicidio_Tunja$`FECHA_DE_ NOTIFICAccCION_ DEL_ EVENTO`)
Suicidio_Tunja=rename(Suicidio_Tunja,FECHA_DE_NOTIFICACION_DEL_EVENTO='FECHA_DE_ NOTIFICAccCION_ DEL_ EVENTO')
Suicidio_Tunja$FECHA_DE_NOTIFICACION_DEL_EVENTO <- as.Date(Suicidio_Tunja$FECHA_DE_NOTIFICACION_DEL_EVENTO,format("%m/%d/%y"))
Suicidio_Tunja$`FECHA_ DEL_ HECHO` <- as.Date(Suicidio_Tunja$`FECHA_ DEL_ HECHO`,format("%m/%d/%y"))

#Arreglo variable Gestante
Suicidio_Tunja$GESTANTE<-gsub("NO APLICA","NO",Suicidio_Tunja$GESTANTE)
Suicidio_Tunja[is.na(Suicidio_Tunja$GESTANTE),"GESTANTE"]<-getmode(Suicidio_Tunja$GESTANTE)
sum(is.na(Suicidio_Tunja$GESTANTE))

#Arreglo variable Barrio de residencia
class(Suicidio_Tunja$`BARRIO_ DE_ RESIDENCIA`)

filter(Suicidio_Tunja, grepl('4',Suicidio_Tunja$`BARRIO_ DE_ RESIDENCIA`))
Suicidio_Tunja=Suicidio_Tunja[!(Suicidio_Tunja$`BARRIO_ DE_ RESIDENCIA`=="44696.0"),]
Suicidio_Tunja=Suicidio_Tunja[!(Suicidio_Tunja$`BARRIO_ DE_ RESIDENCIA`=="44762.0"),]

#UNION DE LOS DATASETS TUNJA
TUNJA<-bind_rows(Suicidio_Tunja,Tunja2)
View(TUNJA)

#VALIDACION  DE HIPOTESIS
#1. EXISTE UNA RELACION ENTRE EL GENERO Y EL GRUPO DE EDAD. ESTO RESPONDE A LA IDEA DE QUE LOS HOMBRES ENTRE 14 Y 24 AÑOS SON MAS PROPENSOS A AUTO INFRINGIRSE LA MUERTE
#2. EL MAYOR NUMERO DE CASOS DE INTENTO DE SUICIDIO EN LA CIUDAD DE TUNJA SE PRESENTA EN MUJERES
getmode(TUNJA$SEXO)
#Conclusion: El resultado arroja que el sexo que mas intenta suicidarse o auto infringirse la muerte es el femenimo, por lo que se confirma la hipotesis planteada




#3. EXISTE UNA RELACION ENTRE EL NUMERO DE INTENTOS DE SUICIDIO Y LO PROBLEMAS SOCIOECONOMICOS. ESTO RESPONDE A LA IDEA DE QUE LAS PERSONAS CON MENOR RENTABILIDAD ECONOMICA INTENTAN SUICIDARSE MAS DE UNA VEZ.
#Se desarrolla con correlacion lineal

#variable1= numero de intentos------------------variable2=problemas economicos
class(TUNJA$`NUMERO_ DE_ INTENTOS`)
class(TUNJA$`PROBLEMAS_ ECONOMICOS`)
TUNJA$`PROBLEMAS_ ECONOMICOS`<-gsub("NO","0",TUNJA$`PROBLEMAS_ ECONOMICOS`)
TUNJA$`PROBLEMAS_ ECONOMICOS`<-gsub("SI","1",TUNJA$`PROBLEMAS_ ECONOMICOS`)
TUNJA$`PROBLEMAS_ ECONOMICOS`<-as.numeric(TUNJA$`PROBLEMAS_ ECONOMICOS`)
sum(is.na(TUNJA$`PROBLEMAS_ ECONOMICOS`))

#Se determina la Distribucion normal de las variables con la funcion "hist" que nos muestra un histograma

hist(TUNJA$`NUMERO_ DE_ INTENTOS`)
hist(TUNJA$`PROBLEMAS_ ECONOMICOS`)

#Pruebas de normalidad para cada variable empleando kolmogorov, teniendo en cuenta que la cantidad de datos es mayor a 50

lillie.test(TUNJA$`NUMERO_ DE_ INTENTOS`)
lillie.test(TUNJA$`PROBLEMAS_ ECONOMICOS`)
#Al ver que el p-value es < 0.05 se determina que las variables no son normales, asi que se aplicaran metodos para no parametricas en este caso metodo "kendall" dentro dela funcion "cor"

 cor(x=TUNJA$`NUMERO_ DE_ INTENTOS`,y=TUNJA$`PROBLEMAS_ ECONOMICOS`,method = "kendall")

 #graficas de dispersion
 plot(x=TUNJA$`NUMERO_ DE_ INTENTOS`,y=TUNJA$`PROBLEMAS_ ECONOMICOS`)
 #el nivel de dispersion entre las variables es demasiado grande, esto se demuestra en la grafica anterio

 
 #Conclusion: El resultado nos permite concluir que  las variables PROBLEMAS_ ECONOMICOS y NUMERO_ DE_ INTENTOS no tienen una correlacion, es decir que esta es nula.
 
 
#4. EL METODO MAS UTILIZADO POR LOS CIUDADANOS DE TUNJA PARA INTENTAR SUICIDARSE  FUE LA INTOXICACION.
Ahorcamineto_SI<-TUNJA %>% filter(AHORCAMIENTO=="SI")%>% select(AHORCAMIENTO)
Ahorcamineto_NO<-TUNJA %>% filter(AHORCAMIENTO=="NO")%>% select(AHORCAMIENTO)
ArmaCortopunzanteS_SI<- TUNJA %>% filter(TUNJA$`ARMA_ CORTOPUNZANTE`=="SI")%>% select(`ARMA_ CORTOPUNZANTE`)
ArmaCortopunzanteS_NO<- TUNJA %>% filter(TUNJA$`ARMA_ CORTOPUNZANTE`=="NO")%>% select(`ARMA_ CORTOPUNZANTE`)
Armafuego_SI<- TUNJA %>% filter(TUNJA$`ARMA _DE_ FUEGO`=="SI")%>% select(`ARMA _DE_ FUEGO`)
Armafuego_NO<- TUNJA %>% filter(TUNJA$`ARMA _DE_ FUEGO`=="NO")%>% select(`ARMA _DE_ FUEGO`)
inmolacion_SI<- TUNJA %>% filter(TUNJA$INMOLACION=="SI")%>% select(INMOLACION)
inmolacion_NO<- TUNJA %>% filter(TUNJA$INMOLACION=="NO")%>% select(INMOLACION)
lanzamientoVacio_SI<- TUNJA %>% filter(TUNJA$`LANZAMIENTO_ AL_ VACIO`=="SI")%>% select(`LANZAMIENTO_ AL_ VACIO`)
lanzamientoVacio_NO<- TUNJA %>% filter(TUNJA$`LANZAMIENTO_ AL_ VACIO`=="NO")%>% select(`LANZAMIENTO_ AL_ VACIO`)
lanzamientoAgua_SI<- TUNJA %>% filter(TUNJA$`LANZAMIENTO_ AL_ AGUA`=="SI")%>% select(`LANZAMIENTO_ AL_ AGUA`)
lanzamientoAgua_NO<- TUNJA %>% filter(TUNJA$`LANZAMIENTO_ AL_ AGUA`=="NO")%>% select(`LANZAMIENTO_ AL_ AGUA`)
lanzamientoVehiculo_SI<- TUNJA %>% filter(TUNJA$`LANZAMIENTO_ A_ VEHICULO`=="SI")%>% select(`LANZAMIENTO_ A_ VEHICULO`)
lanzamientoVehiculo_NO<- TUNJA %>% filter(TUNJA$`LANZAMIENTO_ A_ VEHICULO`=="NO")%>% select(`LANZAMIENTO_ A_ VEHICULO`)
intoxicacion_SI<- TUNJA %>% filter(TUNJA$INTOXICACIONES=="SI")%>% select(INTOXICACIONES)
intoxicacion_NO<- TUNJA %>% filter(TUNJA$INTOXICACIONES=="NO")%>% select(INTOXICACIONES)


tabla<-data.table(METODO=c("AHORCAMIENTO","ARMA CORTOPUNZANTE","ARMA FUEGO","INMOLACION","LANZAMIENTO AL AGUA","LANZAMIENTO AL VACIO","LANZAMIENTO DE VEHICULO","INTOXICACION"), 
                  NUM_PERSONAS_SI=c(count(Ahorcamineto_SI),count(ArmaCortopunzanteS_SI),count(Armafuego_SI),count(inmolacion_SI),count(lanzamientoAgua_SI),count(lanzamientoVacio_SI),count(lanzamientoVehiculo_SI),count(intoxicacion_SI)),
                  NUM_PERSONAS_NO=c(count(Ahorcamineto_NO),count(ArmaCortopunzanteS_NO),count(Armafuego_NO),count(inmolacion_NO),count(lanzamientoAgua_NO),count(lanzamientoVacio_NO),count(lanzamientoVehiculo_NO),count(intoxicacion_NO)))
View(tabla)
#Concñusion: A partir del analisis anteriormente hecho se visaliza que el metodo que mas usaron las personas para intentar suicidarse fue la intoxicacion, por lo cual se valida la hipotesis planteada.





#5. MENOS DEL 40% DEL TOTAL DE LAS PERSONAS DE LA CIUDAD DE TUNJA INTENTARON SUICIDARSE MAS DE UNA VEZ

#TOTAL->242  personas o registros es decir 100%
 TOTAL<- count(TUNJA)
# Numero de personas que intentaron suicidarse mas de una vez
NUM<- TUNJA %>% filter(TUNJA$`NUMERO_ DE_ INTENTOS`>1)%>% select(`NUMERO_ DE_ INTENTOS`)
T_person<-count(NUM)
View(T_person)
(T_person*100)/TOTAL
#Conclusion: Teniendo en cuenta  que el porcentaje de personas que intentaron suicidarse mas de una vez es de 34.71074 % se valida la hipotesis planteada.





#6. EXISTE UNA RELACION ENTRE EL RANGO DE EDAD Y EL NUMERO DE INTENTOS DE SUICIDIO. ESTO RESPONDE A LA IDEA QUE TENIENDO EN CUENTA LA EDAD LA PERSONA PUEDE INTENTAR SUICIDARSE MAS DE UNA VEZ.
 
# se procede a crear los rangos de edad
summary(TUNJA$EDAD_DE_LA_VICTIMA)
TUNJA<-TUNJA %>% mutate(RANGO_EDAD=cut(TUNJA$EDAD_DE_LA_VICTIMA,breaks = c(6,11,18,27,60,80)),ETAPA=cut(TUNJA$EDAD_DE_LA_VICTIMA,breaks = c(6,11,18,27,60,80),labels=c("INFANCIA","ADOLESCENCIA","JUVENTUD","ADULTEZ","OERSONA MAYOR")))
View(TUNJA)

TUNJA$RANGO_EDAD<-as.numeric(TUNJA$RANGO_EDAD)
TUNJA$ETAPA<-as.character(TUNJA$ETAPA)
class(TUNJA$RANGO_EDAD)

#variable dependiente =  intentos previos
#variable independiente= rango de edad

# Tabla de contingencia
class(TUNJA$`NUMERO_ DE_ INTENTOS`)

tabla_contingencia<-table(TUNJA$`INTENTOS_ PREVIOS`,TUNJA$RANGO_EDAD)
tabla_contingencia

#Tabla de frecuencias observadas

tabla_frecuencias_observadas<-table(TUNJA$`INTENTOS_ PREVIOS`,TUNJA$RANGO_EDAD)

addmargins(tabla_frecuencias_observadas)

# Prueba chi cuadrado tabla de contingencia

chisq.test(tabla_contingencia)

chisq.test(tabla_contingencia, simulate.p.value = TRUE)
#Conlusion: Teniendo en cuenta el valor del p-value se Rechaza la hipotesis estadistica H1, por lo cual se asegura que las dos variables son independientes.
