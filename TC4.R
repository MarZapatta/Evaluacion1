rm(list = ls())
getwd()
setwd("E:/Escritorio/TClase4/")
getwd()

#SETIEMBRE 2018
#Cargamos la data
OsinergSet2k18 <- read.table("201809_TABLA04_SICLI.txt", header = TRUE,sep = "\t",
                               col.names = c("CodEmpresa","Suministro", "PuntoSuministro","Fecha","RegistroActiva","RegistroPasiva","Periodo"),
                               colClasses = c("factor","factor","factor","character","numeric", "numeric", "character"))
view(OsinergSet2k18)
class(OsinergSet2k18)
str(OsinergSet2k18)
dim(OsinergSet2k18)
help("POSIXct")

#### Conversion de la variable fecha ####
OsinergSet2k18$Fecha
library(dplyr)
library(ggplot2)
library(lubridate)
library(help = "lubridate")
library(gridExtra)
library(stringi)
help("ymd_hm")

# Creamos la variable FechaDate para recuperar la cadena que se tiene en el 
# data frame original 
OsinergSet2k18$FechaDate <- ymd_hm(OsinergSet2k18$Fecha)
class(OsinergSet2k18$FechaDate)

# Usando libreria base  1raforma
OsinergSet2k18$year <- format(OsinergSet2k18$FechaDate,"%Y")
OsinergSet2k18$month <- format(OsinergSet2k18$FechaDate,"%m")
OsinergSet2k18$day <- format(OsinergSet2k18$FechaDate,"%d")
OsinergSet2k18$hour <- format(OsinergSet2k18$FechaDate,"%H")
OsinergSet2k18$minute <- format(OsinergSet2k18$FechaDate,"%M")

# Usando funciones de lubridate 2daforma
year(OsinergSet2k18$FechaDate)
day(OsinergSet2k18$FechaDate)
month(OsinergSet2k18$FechaDate)
hour(OsinergSet2k18$FechaDate)
minute(OsinergSet2k18$FechaDate)

summary(OsinergSet2k18$FechaDate)
ggplot(OsinergSet2k18,aes(x=OsinergSet2k18$year))

# Diagrama de dispersion del data frame en toda la linea del tiempo
head(OsinergSet2k18, n = 1)$FechaDate   # primer dia de la data 
tail(OsinergSet2k18, n =1)$FechaDate # ultimo dia de la data 

#MAXIMO Y MINIMO
min(OsinergSet2k18$RegistroActiva)
max(OsinergSet2k18$RegistroActiva)

min(OsinergSet2k18$RegistroPasiva, na.rm = TRUE)
max(OsinergSet2k18$RegistroPasiva, na.rm = TRUE)

#HISTOGRAMAS VARIABLES PSUMINISTRO Y CODEMPRESA
hist(table(OsinergSet2k18$PuntoSuministro), col = "yellow" , main = "Histograma para la variable PSuministro " , xlab = "PSuministro" , ylab = "Frecuencia")
hist(table(OsinergSet2k18$CodEmpresa), col = "red" , main = "Histograma para la variable Empresa " , xlab = "CodEmpresa" , ylab = "Frecuencia")

#DIAGRAMA DE BARRAS PARA LA VARIABLE COD_EMPRESA
barplot(table(OsinergSet2k18$CodEmpresa), col = c("yellow","orange","green", "red"),xlab = "CALIFICACION",ylab = "FRECUENCIAS ABSOLUTAS" ,main = "Diagrama de sectores para la variable Calificacion")

#Gráfica de Energía Pasiva VS CodEmpresa="CEEP" en base al Suministro 18

CodEmpresa18<-OsinergSet2k18[OsinergSet2k18$CodEmpresa=="CEEP",]
Plt1<-ggplot(CodEmpresa18, aes(x=FechaDate, y=RegistroPasiva, group = Suministro, colour = Suministro )) + 
  geom_line()  + 
  theme_minimal()

grid.arrange(Plt1,nrow=1)

#### PARA SETIEMBRE 2019 #####

#SETIEMBRE 2019
#Cargamos la data
OsinergSet2k19 <-read.table("201909_TABLA4.txt", header = TRUE,sep = "\t",
                            colClasses = c("factor","factor","factor","character","character", "character", "character"),)


# Verificamos la clase de cada una de las variables (columnas)
lapply(OsinergSet2k19, class)    

# Cambiamos las comas (decimales) por puntos decimales mediante la creacion de nuevas columnas
# Estas columas nuevas son la representacion numerica (con punto decimal) de las columnas ENERG_ACTV y ENERG_REAC
OsinergSet2k19$ENERG_ACTV_Numeric <- as.numeric(sub(",", ".", OsinergSet2k19$ENERG_ACTV, fixed = TRUE))
OsinergSet2k19$ENERG_REAC_Numeric <- as.numeric(sub(",", ".", OsinergSet2k19$ENERG_REAC, fixed = TRUE))

# Verificamos la clase de cada una de las columnas (variables)
lapply(OsinergSet2k19, class)

# Eliminamos dos variables que ya o sirven en el formato incial
OsinergSet2k19 <- OsinergSet2k19[, -c(5,6)]

# Como deseamos porciones (subconjuntos) de la variable FECHA (es un vector de caracteres)
# usamos la funcion substr (la cual sirve para muchisimos escenarios)
help("substr")
# Despues de leer la ayuda de la funcion substr, debemos contar
# la cantidad de caracteres que forman la cadena de caracteres FECHA
nchar(OsinergSet2k19$FECHA[1])
# Esta funcion nos dice que cada elemento de la columna FECHA es una cadena
# de caracteres con 30 elementos. 

# Con la informacion anterior, procedemos a elegir las porciones adeciadas para
# definir nuestras variables de interes
help(substr)
OsinergSet2k19$diaMesAnio <- substr(x = OsinergSet2k19$FECHA,start = 1,stop = 8)
OsinergSet2k19$hora<- substr(x = OsinergSet2k19$FECHA,start = 10,stop = 11)
OsinergSet2k19$minuto <- substr(x = OsinergSet2k19$FECHA,start = 13,stop = 14)
OsinergSet2k19$Meridiano <- substr(x = OsinergSet2k19$FECHA,start = 29,stop = 30)
OsinergSet2k19$dia<- day(OsinergSet2k19$FECHA)
OsinergSet2k19$mes <- month(OsinergSet2k19$diaMesAnio)

# De esta manera se construyen variables que tienen la informacion de la fecha
# y del tiempo. OJO que las horas estan en formato de 12 horas pues se tiene una variable
# que indica el meridiano (AM/PM), es decir, tienes que corregir la variable hora para que este en un
# formato de 24 horas y asi puedas procesar de mejor manera la  informacion

# Ten cuenta lo siguiente para crear una variable hora en formato de 24 horas (Hora24)
OsinergSet2k19$Hora24 <- ifelse(OsinergSet2k19$Meridiano=="AM", as.integer(OsinergSet2k19$hora) ,as.integer(OsinergSet2k19$hora)+12  )

#Media , varianza y desviacion estandar Para el Registro Activo y Pasivo
media1 <- mean(OsinergSet2k19$ENERG_ACTV_Numeric , na.rm = TRUE)
media2 <- mean(OsinergSet2k19$ENERG_REAC_Numeric , na.rm = TRUE)
var(OsinergSet2k19$ENERG_ACTV_Numeric , na.rm = TRUE)
sd(OsinergSet2k19$ENERG_ACTV_Numeric , na.rm = TRUE)
var(OsinergSet2k19$ENERG_REAC_Numeric , na.rm = TRUE)
sd(OsinergSet2k19$ENERG_REAC_Numeric , na.rm = TRUE)

summary(OsinergSet2k19$ENERG_ACTV_Numeric)
summary(OsinergSet2k19$ENERG_REAC_Numeric)

OsinergSet2k19$COD_EMPRESA
str(OsinergSet2k19$COD_EMPRESA)
OsinergSet2k19$COD_SUMINISTRO_USUARIO
str(OsinergSet2k19$COD_SUMINISTRO_USUARIO)
OsinergSet2k19$COD_PUNTO_SUMINISTRO
str(OsinergSet2k19$COD_PUNTO_SUMINISTRO)

#MAXIMO Y MINIMO
min(OsinergSet2k19$ENERG_ACTV_Numeric)
max(OsinergSet2k19$ENERG_ACTV_Numeric)

min(OsinergSet2k19$ENERG_REAC_Numeric, na.rm = TRUE)
max(OsinergSet2k19$ENERG_REAC_Numeric, na.rm = TRUE)

#HISTOGRAMAS VARIABLES PSUMINISTRO Y CODEMPRESA
hist(table(OsinergSet2k19$COD_PUNTO_SUMINISTRO), col = "yellow" , main = "Histograma para la variable PSuministro " , xlab = "PSuministro" , ylab = "Frecuencia")
hist(table(OsinergSet2k19$COD_EMPRESA), col = "red" , main = "Histograma para la variable Empresa " , xlab = "CodEmpresa" , ylab = "Frecuencia")

#DIAGRAMA DE BARRAS PARA LA VARIABLE COD_EMPRESA
barplot(table(OsinergSet2k19$COD_EMPRESA), col = c("yellow","orange","green", "red"),xlab = "CALIFICACION",ylab = "FRECUENCIAS ABSOLUTAS" ,main = "Diagrama de sectores para la variable Calificacion")


#Gráfica de Energía Pasiva VS CodEmpresa="ATRE" en base al Suministro 19

CodEmpresa19<-OsinergSet2k19[OsinergSet2k19$COD_EMPRESA=="ATRE",]
Plt2<-ggplot(CodEmpresa19, aes(x=Hora24, y=ENERG_REAC_Numeric, group = COD_PUNTO_SUMINISTRO, colour = COD_PUNTO_SUMINISTRO )) + 
  geom_line()  + 
  theme_minimal()

grid.arrange(Plt2,nrow=1)







