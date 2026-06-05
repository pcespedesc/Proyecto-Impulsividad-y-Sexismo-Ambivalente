#1. INSTALACIÓN DE PAQUETES 

#Leer archivos y manipulación de datos 
install.packages("haven")
install.packages("dplyr")
#Análisis descriptivos
install.packages("summarytools")
install.packages("crosstable")
install.packages("flextable")
#Outliers
install.packages("Routliers")
#AFC/SEM
install.packages("semPlot")
install.packages("lavaan")
#TRI
install.packages("mirt")
install.packages("psych")
#PLS-SEM
install.packages("seminr")

#Carga de librerías 
library(haven)
library(summarytools)
library(crosstable) 
library(flextable)
library(dplyr)
library(Routliers)
library(seminr)
library(mirt)
library(psych)
library(semPlot)
library(lavaan)

#2 DEPURACIÓN DE LA BASE DE DATOS 

#Carga de la base de datos desde SPSS
data <- read_sav("Sexismo.sav")

# Exploración de la estructura inicial y las variables de la base de datos 
str(data)
colnames(data)

#Selección de variables

#Selección por ítems (para AFC y TRI)
#Items de impulsividad
items_eip <- c(
  "EIP01","EIP02","EIP03","EIP04","EIP05",
  "EIP06","EIP07","EIP08","EIP09","EIP10",
  "EIP11","EIP12","EIP13","EIP14","EIP15")

#Items de Sexismo ambivalente (ESA) y hostil y benevolente 
items_esa <- c("ESA01","ESA02","ESA03","ESA04","ESA05",
  "ESA06","ESA07","ESA08","ESA09","ESA10",
  "ESA11","ESA12","ESA13","ESA14","ESA15",
  "ESA16","ESA17","ESA18","ESA19","ESA20",
  "ESA21","ESA22")
items_hostil <- c("ESA02","ESA04","ESA05","ESA07","ESA10",
                    "ESA11","ESA14","ESA15","ESA16","ESA18","ESA21")
items_benev  <- c("ESA01","ESA03","ESA06","ESA08","ESA09",
                    "ESA12","ESA13","ESA17","ESA19","ESA20","ESA22")

#Subdimensiones del sexismo benevolente (Vaamonde y Omar, 2012)
items_paternalismo <- c("ESA03","ESA09","ESA17","ESA20")  # Paternalismo Protector
items_difgenero    <- c("ESA08","ESA19","ESA22")           # Diferenciación de Género
items_intimidad    <- c("ESA01","ESA06","ESA12","ESA13")   # Intimidad Heterosexual

#Puntajes totales (para PLS-SEM)

# EIP = Escala de Impulsividad de Plutchik (variable independiente)
# SEXHOS = Sexismo Hostil (variable dependiente)
# SEXBENE = Sexismo Benevolente (variable dependiente)
# PATERPRO, DIGENERO, INTIMIDAD = Subdimensiones del sexismo benevolente
# EDAD = Covariable
# SEXO, GRUPO = Variables moderadoras

puntotal<- c("EIP","SEXHOS","SEXBENE",
             "PATERPRO","DIGENERO","INTIMIDAD",
             "EDAD","SEXO","GRUPO")


datos <- data[, c(puntotal, items_eip, items_esa)]

# Se recodifican SEXO y GRUPO como factores (variables categóricas nominales) para que R no las tome como números sino como categorías
datos$SEXO <- factor(datos$SEXO,
                     levels = c(0,1),labels = c("Mujer","Hombre"))
datos$GRUPO <- factor(datos$GRUPO,
                      levels = c(1,2),labels = c("Recluso","Estudiante"))

#Ahora dejamos las que son numéricas y se deben tratar como tal
cols_num <- c("EIP","SEXHOS","SEXBENE",
              "PATERPRO","DIGENERO","INTIMIDAD","EDAD",items_eip, items_esa)
datos[cols_num] <- lapply(datos[cols_num], as.numeric)
