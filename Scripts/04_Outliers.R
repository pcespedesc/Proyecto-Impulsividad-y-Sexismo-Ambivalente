#5.ANÁLISIS DE VALORES ATÍPICOS (OUTLIERS)

#MODELO 1: OUTLIERS UNIVARIADOS

#Impulsividad
Out_EIP <- outliers_mad(datos_m1$EIP, threshold = 3)
plot_outliers_mad(Out_EIP, datos_m1$EIP)
# Resultado = no hay outliers

#Sexismo hostil
Out_SEXHOS <- outliers_mad(datos_m1$SEXHOS, threshold = 3)
plot_outliers_mad(Out_SEXHOS, datos_m1$SEXHOS)
# Resultado = no hay outliers

#Sexismo benevolente
Out_SEXBENE <- outliers_mad(datos_m1$SEXBENE, threshold = 3)
plot_outliers_mad(Out_SEXBENE, datos_m1$SEXBENE)
# Resultado = 1 outlier 

#Se identifica dónde está el outlier
Out_SEXBENE$outliers_pos # Resultado = fila 55

#Se ven sus valores en todas las variables
datos_m1[Out_SEXBENE$outliers_pos, ]
#Decisión = conservarlo porque el valor de sexismo benevolente está dentro del rango y representa un perfil teóricamente plausible

#MODELO 1: OUTLIERS MULTIVARIADOS
#Aquí los casos atípicos son detectados considerando la combinación de todas las variables 

vars_m1 <- data.frame(
  EIP = as.numeric(datos_m1$EIP),
  SEXHOS = as.numeric(datos_m1$SEXHOS),
  SEXBENE = as.numeric(datos_m1$SEXBENE)
)
str(vars_m1)

MUL_OUT_m1 <- outliers_mcd(vars_m1, h = .75)
plot_outliers_mcd(MUL_OUT_m1, vars_m1)

# Ver qué filas son
MUL_OUT_m1$outliers_pos

# Ver sus valores
datos_m1[MUL_OUT_m1$outliers_pos, ]

# Resultado = 7 outliers multivariados 
# Decisión = Es plausible tener sexismo hostil bajo y benevolente alto, por lo que se conservan

#MODELO 2: OUTLIERS UNIVARIADOS

#PATERNALISMO PROTECTOR
Out_PATERPRO <- outliers_mad(datos_m2$PATERPRO, threshold = 3)
plot_outliers_mad(Out_PATERPRO, datos_m2$PATERPRO)
# Resultado = no hay outliers 

#DIFERENCIACIÓN DE GÉNERO
Out_DIGENERO <- outliers_mad(datos_m2$DIGENERO, threshold = 3)
plot_outliers_mad(Out_DIGENERO, datos_m2$DIGENERO)
# Resultado = 4 outliers 

#INTIMIDAD
Out_INTIMIDAD <- outliers_mad(datos_m2$INTIMIDAD, threshold = 3)
plot_outliers_mad(Out_INTIMIDAD, datos_m2$INTIMIDAD)
# Resultado = 1 outlier 

#Se revisan los casos detectados
Out_DIGENERO$outliers_pos
datos_m2[Out_DIGENERO$outliers_pos, ]

Out_INTIMIDAD$outliers_pos
datos_m2[Out_INTIMIDAD$outliers_pos, ]

# Decisión: El caso 24 aparece en ambos análisis y tiene SEXBENE = 0, valor imposible porque el mínimo posible de la escala es =6. 
# Es error de registro → se elimina.
# Los otros 3 outliers de DIGENERO tienen valor = 0, dentro del rango posible (0-9), se conservan.

#Se elimina el caso 24
datos_m2 <- datos[complete.cases(datos[, c(
  "EIP","PATERPRO","DIGENERO","INTIMIDAD"
)]), ]
datos_m2 <- datos_m2[-24, ]

# Se verifica que el mínimo de SEXBENE ahora sea 6
nrow(datos_m2)
summary(datos_m2$SEXBENE)

#MODELO 2: OUTLIERS MULTIVARIADOS 
vars_m2 <- data.frame(
  EIP = as.numeric(datos_m2$EIP),
  PATERPRO = as.numeric(datos_m2$PATERPRO),
  DIGENERO = as.numeric(datos_m2$DIGENERO),
  INTIMIDAD = as.numeric(datos_m2$INTIMIDAD)
)

MUL_OUT_m2 <- outliers_mcd(vars_m2, h = .75)
plot_outliers_mcd(MUL_OUT_m2, vars_m2)

# Ver qué filas son
MUL_OUT_m2$outliers_pos

# Ver sus valores
datos_m2[MUL_OUT_m2$outliers_pos, ]

# Resultado = 8 outliers multivariados 
# Decisión = Se conservan porque tienen valores dentro del rango posible de las escalas 
# y también representan perfiles teóricamente plausibles 
