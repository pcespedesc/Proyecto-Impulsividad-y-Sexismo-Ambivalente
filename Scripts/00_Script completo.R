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

#3.MANEJO DE DATOS PERDIDOS 

# Se identifican cuántos y cuáles son los datos faltantes por variable
colSums(is.na(datos))
lapply(datos, function(x) which(is.na(x)))

#Se dejan solo las filas que tienen datos completos según los modelos (dados por los objetivos)

#MODELO 1 (Impulsividad y Escalas totales de sexismo hostil y benevolente)
#MODELO 2 (Impulsividad y Subdimensiones del sexismo benevolente)

# Se eliminan los casos en los que una persona tiene un dato perdido según el modelo
datos_m1 <- datos[complete.cases(datos[, c("EIP","SEXHOS","SEXBENE")]), ]
datos_m2 <- datos[complete.cases(datos[, c("EIP","PATERPRO","DIGENERO","INTIMIDAD")]), ]

cat("N modelo 1:", nrow(datos_m1), "\n")
cat("N modelo 2:", nrow(datos_m2), "\n")

#4.ANÁLISIS DESCRIPTIVOS 

# Resumen de cada base 
dfSummary(datos_m1[, c("EIP","SEXHOS","SEXBENE","EDAD","SEXO","GRUPO")])
dfSummary(datos_m2[, c("EIP","PATERPRO","DIGENERO","INTIMIDAD","EDAD","SEXO","GRUPO")])

#Descriptivos por grupo y sexo - Modelo 1 (objetivo específico 1)

#Crosstable por grupo
crosstable(datos_m1,
           c(EIP, SEXHOS, SEXBENE, EDAD),
           by = GRUPO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)
#Los reclusos tienen más edad (media de 36 años) que los estudiantes (media de 22 años) y mayores puntajes en ambos tipos de sexismo que los estudiantes
#La impulsividad es similar entre ambos grupos. 

#Crosstable por sexo
crosstable(datos_m1,
           c(EIP, SEXHOS, SEXBENE, EDAD),
           by = SEXO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)
#Hombres tienen puntajes ligeramente superiores en sexismo, aunque la diferencia es mínima
#La impulsividad es practicamente igual. En cuanto a la edad los hombres en promedio son mayores (29 vs 23)

#Descriptivos por grupo y sexo - Modelo 2 

#Crosstable por grupo
crosstable(datos_m2,
           c(EIP, PATERPRO, DIGENERO, INTIMIDAD, EDAD),
           by = GRUPO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)
#Los reclusos puntúan consistentemente más alto en las tres subdimensiones del sexismo benevolente. 
#La diferencia más grande es en la subdimensión de paternalismo protector. 

#Crosstable por sexo
crosstable(datos_m2,
           c(EIP, PATERPRO, DIGENERO, INTIMIDAD, EDAD),
           by = SEXO,
           showNA = "no",
           funs = c(mean, sd)) %>%
  as_flextable(keep_id = FALSE)
#Las mujeres puntúan más bajo que los hombres en las subdimensiones de paternalismo e intimidad, pero ligeramente más alto
#en diferenciación de género. Aunque las diferencias son pequeñas. 

#Histogramas exploratorios de distribución de cada variable
par(mfrow = c(2,3))
hist(datos_m1$EIP,     main = "EIP",      xlab = "") #La mayoría de valores se concentran entre 0.5 y 1.5, con pocos casos en los extremos. 
hist(datos_m1$SEXHOS,  main = "SEXHOS",   xlab = "") #Los valores se concentran entre 10 y 25, con una concentración en los valores medios. 
hist(datos_m1$SEXBENE, main = "SEXBENE",  xlab = "") #Es la que más se aproxima a una distribución normal, con una mayor frecuencia entre 16 y 24
hist(datos_m2$PATERPRO,main = "PATERPRO", xlab = "") #Mayor concentración entre 5 y 9 
hist(datos_m2$DIGENERO,main = "DIGENERO", xlab = "") #Concentración principal entre 4 y 7
hist(datos_m2$INTIMIDAD,main = "INTIMIDAD",xlab = "") #Distribución cercana a la normalidad  con una mayor concentración entre 5 y 7
par(mfrow = c(1,1))

#Boxplots para explorar diferencias entre grupos y sexo 
boxplot(EIP ~ SEXO * GRUPO,      data = datos_m1, main = "EIP por sexo y grupo") #Medianas muy similares entre estudiantes y mujer reclusa, pero hombre recluso presenta la mediana más baja, aunque esta diferencia puede no ser muy grande
boxplot(SEXHOS ~ SEXO * GRUPO,   data = datos_m1, main = "Sexismo Hostil") #Los reclusos parecen presentar mayores niveles de sexismo hostil que los estudiantes. La mediana más alta es mujer reclusa mientras que la mediana más baja es mujer estudiante
boxplot(SEXBENE ~ SEXO * GRUPO,  data = datos_m1, main = "Sexismo Benevolente") #Las mujeres reclusas presentan la mediana más alta y los dos grupos de estudiantes muestran medianas menores. 
boxplot(PATERPRO ~ SEXO * GRUPO, data = datos_m2, main = "Paternalismo Protector") #Las mujeres reclusas tienen la mediana más alta, seguido de los hombres reclusos 
boxplot(DIGENERO ~ SEXO * GRUPO, data = datos_m2, main = "Diferenciación de Género") #Se destacan los outliers. Los reclusos tienen mayor diferenciación de género que los estudiantes. Entre estudiantes, los hombres muestran niveles más bajos
boxplot(INTIMIDAD ~ SEXO * GRUPO,data = datos_m2, main = "Intimidad Heterosexual") #Las medianas son muy parecidas en los grupos

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
# Ver qué filas son
MUL_OUT_m2$outliers_pos

# Ver sus valores
datos_m2[MUL_OUT_m2$outliers_pos, ]

# Resultado = 8 outliers multivariados 
# Decisión = Se conservan porque tienen valores dentro del rango posible de las escalas 
# y también representan perfiles teóricamente plausibles 

# 6. ANÁLISIS FACTORIAL CONFIRMATORIO (AFC)
# Con el propósito de verificar la estructura según Vaamonde y Omar (2012)
# Es decir: Modelo con 3 subdimensiones para el sexismo benevolente
#           - Paternalismo Protector:   ESA03, ESA09, ESA17, ESA20
#           - Diferenciación de Género: ESA08, ESA19, ESA22
#           - Intimidad Heterosexual:   ESA01, ESA06, ESA12, ESA13
# Mientras que el sexismo hostil se trata como factor unitario (11 ítems)

#En EIP se asume de 1 factor porque se cuenta con el puntaje total en la base de datos

#Solo se toman los casos con respuestas completas (sin N/A)
datos_afc_eip <- data[complete.cases(data[, items_eip]), items_eip]
datos_afc_esa <- data[complete.cases(data[, items_esa]), items_esa]

#Conversión de ítems a datos númericos y que sean tratados como tal
datos_afc_eip <- lapply(datos_afc_eip, as.numeric) %>% as.data.frame()
datos_afc_esa <- lapply(datos_afc_esa, as.numeric) %>% as.data.frame()

#6.1 ANÁLISIS FACTORIAL DE EIP

modelo_afc_eip <- 'Impulsividad =~ EIP01 + EIP02 + EIP03 + EIP04 + EIP05 +
                  EIP06 + EIP07 + EIP08 + EIP09 + EIP10 +
                  EIP11 + EIP12 + EIP13 + EIP14 + EIP15'

fit_afc_eip <- cfa(
  model     = modelo_afc_eip,
  data      = datos_afc_eip,
  estimator = "WLSMV",
  ordered   = TRUE)

summary(fit_afc_eip, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit_afc_eip, c("cfi", "tli", "rmsea", "wrmr"))

semPaths(fit_afc_eip, what = "std", layout = "tree",
        title = FALSE, style = "ram",
        main = "AFC - Impulsividad (EIP)")
#La mayoría de los ítems tienen cargas factoriales significativas y en la dirección esperada, con excepción de el ítem 4, 6 y 11 que mostraron cargas bajas o no significativas.
#Sin embargo, dado que se trabajó con el puntaje total no se eliminan ítems. 

# 6.2 AFC SEXISMO 2 FACTORES  

modelo_afc_esa_2f <- '
  SexHostil =~ ESA02 + ESA04 + ESA05 + ESA07 + ESA10 +
               ESA11 + ESA14 + ESA15 + ESA16 + ESA18 + ESA21
 
  SexBenev  =~ ESA01 + ESA03 + ESA06 + ESA08 + ESA09 +
               ESA12 + ESA13 + ESA17 + ESA19 + ESA20 + ESA22'

fit_afc_esa_2f <- cfa(
  model     = modelo_afc_esa_2f,
  data      = datos_afc_esa,
  estimator = "WLSMV",
  ordered   = TRUE)

summary(fit_afc_esa_2f, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit_afc_esa_2f, c("cfi", "tli", "rmsea", "wrmr"))

semPaths(fit_afc_esa_2f, what = "std", layout = "tree",
         title = FALSE, style = "ram",
         main = "AFC - ESA (2 factores: Hostil y Benevolente)")
#El 0.46 entre los dos factores indica que ambos factores están moderadamente correlacionados. Lo cual es coherente con la teoría de Glick y Fiske (1996)
#Todos los ítems del factor hostil son positivos y significativos
#Sin embargo, ESA 13 tiene una carga negativa de 0.44, lo que muestra que este ítem funciona en dirección opuesta al resto de ítems y ESA06 mostró una carga baja de 0.21.Dado que se trabajó
# con el puntaje total provisto en la base de datos, no se procedió a eliminar ítems; estos hallazgos se reportan como una limitación del instrumento en esta muestra específica.
       
modelo_afc_benev_3f <- 'Paternalismo =~ ESA03 + ESA09 + ESA17 + ESA20
  DifGenero    =~ ESA08 + ESA19 + ESA22
  Intimidad    =~ ESA01 + ESA06 + ESA12 + ESA13
  Paternalismo ~~ DifGenero + Intimidad
  DifGenero ~~ Intimidad'

items_benev_3f  <- c("ESA01","ESA03","ESA06","ESA08","ESA09",
                 "ESA12","ESA13","ESA17","ESA19","ESA20","ESA22")

fit_afc_benev_3f <- cfa(
  model     = modelo_afc_benev_3f,
  data      = datos_afc_esa[, items_benev_3f],
  estimator = "WLSMV",
  ordered   = TRUE)

summary(fit_afc_benev_3f, fit.measures = TRUE, standardized = TRUE)

fitMeasures(fit_afc_benev_3f, c("cfi", "tli", "rmsea", "wrmr"))

semPaths(fit_afc_benev_3f,
         what = "std",
         layout = "tree",
         style = "ram",
         nCharNodes = 6,
         residuals = FALSE,
         title = TRUE,
         main = "AFC - Sexismo Benevolente (3 factores correlacionados)")
#El AFC confirmó la estructura de tres factores correlacionados del sexismo benevolente propuesta por Vaamonde y Omar (2012), con índices de ajuste satisfactorios (CFI = .978, TLI = .970, RMSEA = .056, WRMR = .906). 
#Las tres subdimensiones mostraron correlaciones altas entre sí (r = .62 a .87), lo que respalda su pertenencia a un constructo común. Las cargas factoriales fueron en general adecuadas, con excepción de ESA13 y ESA06, que presentaron el mismo comportamiento atípico identificado en el modelo bifactorial, y se reportan como limitación del instrumento en esta muestra.

#6.4 AFC SUBDIMENSIONES COMO INDICADORES DEL SEXISMO AMBIVALENTE 
modelo_afc_benev_2orden <- 'Paternalismo =~ ESA03 + ESA09 + ESA17 + ESA20
  DifGenero    =~ ESA08 + ESA19 + ESA22
  Intimidad    =~ ESA01 + ESA06 + ESA12 + ESA13
  SexBenev =~ Paternalismo + DifGenero + Intimidad'
fit_afc_benev_2orden <- cfa(
  model     = modelo_afc_benev_2orden,
  data      = datos_afc_esa[, items_benev],
  estimator = "WLSMV",
  ordered   = TRUE)

#Aquí sale el aviso de que hay varianzas negativas en los factores latentes, lo cual indica problemas con el modelo, así que se opta
#por conservar la estructura de tres factores correlacionados. Esto se puede explicar por las correlaciones tan altas entre sí de las subdimensiones, lo cual evidencia que comparten una gran cantidad de varianza
#y dificultan la identificación de un factor general independiente. 

#7. TEORÍA DE RESPUESTA AL ÍTEM - MODELO DE SAMEJIMA

#Categorías de respuesta de ítems de la EIP
sapply(datos_afc_eip, function(x) sort(unique(na.omit(x)))) 

#modelo
fit_grm_eip <- mirt(
  data     = datos_afc_eip,
  model    = 1,
  itemtype = "graded",
  SE       = TRUE,
  verbose  = FALSE)

#resumen del modelo
summary(fit_grm_eip)

#parámetros
params_grm <- coef(fit_grm_eip, IRTpars = TRUE, simplify = TRUE)
round(params_grm$items, 3)
#EIP04 y EIP06 tienen discriminaciones negativas, lo que confirma lo observado en el AFC. Así como EIP15 que tiene una discriminación muy baja. 
#En EIP04, EIP06 y EIP11 los umbrales se comportan de manera anormal, lo que indica que los ítems no están funcionando bien. 
#Gráficos
plot(fit_grm_eip, type = "trace",
     main = "Curvas Características de los Ítems - EIP")
# Ítems como el 02, 05 y 10 muestran curvas ordenadas en donde cada categoría domuna en un rango de theta, pero EIP06 y EIP11 muestran curvas casi planas y superpuestas confirmando que no discriminan. 
# EIP04 muestra un patrón invertido. 
plot(fit_grm_eip, type = "infotrace",
     main = "Información por Ítem - EIP")
#EIP 04, 06, 09 y 11 aportan poca información o casi nula para estimar el rasgo, mientras que EIP02 y EIP10 son los más informativos complementados por EIP05 que aporta información sobre personas con poco nivel de rasgo 
plot(fit_grm_eip, type = "info",
     main = "Información Total del Test - EIP")
#Se puede ver que el test mide con mayor precisión a personas con impulsividad media-alta. 
plot(fit_grm_eip, type = "SE",
     main = "Error Estándar - EIP")
#El error mínimo se alcanza entre θ = -1 y 2, lo cual es consistente con el gráfico anterior.

# GRM para la Escala de Sexismo Ambivalente

#Sexismo hostil 

#Categorías de respuesta de ítems 
sapply(datos_afc_esa[, items_hostil], function(x) sort(unique(na.omit(x))))

#modelo
fit_grm_hostil <- mirt(
  data     = datos_afc_esa[, items_hostil],
  model    = 1,
  itemtype = "graded",
  SE       = TRUE,
  verbose  = FALSE)

#resumen del modelo
summary(fit_grm_hostil)

#parámetros
round(coef(fit_grm_hostil, IRTpars = TRUE, simplify = TRUE)$items,
  3)
#Todos los ítems tienen discriminación positiva a diferencia de EIP. ESA11 es el que más discrimina, seguido de ESA14, ESA05 Y ESA10. Los ítems con discriminación más baja 
#son el ESA04 y ESA18, que aunque distingan con menos precisión siguen funcionando. 
#Asimismo, todos tienen umbrales ordenados, e incluso los umbrales de b1 son muy negativos lo que significa que incluso personas con niveles bajos de sexismo hostil tienen alta probabilidad
#de responder en categorías distintas a la mínima, lo que va en concordancia con los descriptivos. 
       
#Gráficos

plot(fit_grm_hostil, type = "trace",
     main = "CCI - Sexismo Hostil")
#Todos los ítems muestran curvas ordenadas y separadas. SA14 y ESA05 tienen las curvas más bien definidas y separadas, consistente con sus altos parámetros de discriminación. 
#ESA04 y ESA18 muestran curvas algo más superpuestas, coherente con su menor discriminación.
plot(fit_grm_hostil, type = "info",
     main = "Información - Sexismo Hostil")
#La curva de información tiene un rango amplio de theta entre -2 y 2 con un pico máximo cerca de 0, lo que indica que la escala mide con precisión a personas 
#con niveles bajos como medios de sexismo hostil. 

#Sexismo benevolente 
#Categorías de respuesta de ítems 
sapply(datos_afc_esa[, items_benev], function(x) sort(unique(na.omit(x))))

#modelo
fit_grm_benev <- mirt(
  data     = datos_afc_esa[, items_benev],
  model    = 1,
  itemtype = "graded",
  SE       = TRUE,
  verbose  = FALSE)

#resumen del modelo
summary(fit_grm_benev)

#parámetros
round(coef(fit_grm_benev, IRTpars = TRUE, simplify = TRUE)$items,
  3)
#ESA12 es el ítem que más discrimina y tiene umbrales ordenados, siendo practicamente el que domina la medición del factor, lo cual es consistente con su carga de 0.92 en el AFC
#ESA09, ESA22, ESA01 funcionan bien y el resto tienen discriminación baja pero positiva a excepción del ESA13 que su valor de a=-1.13 y umbrales invertidos lo que confirma por tercera vez que funciona en dirección opuesta.
#ESA06 tiene discriminación casi nula, tambien consistente con lo observado anteriormente. Al igual que umbrales que se comportan de manera anómala.  
#ESA09 Y ESA08 tienen umbrales muy negativos, lo que significa que se necesita un nivel muy bajo de sexismo benevolente para responder en la categoría mínima, por lo que casi toda la muestra tiende a responder en categorías medias o altas. 

#Gráficos

plot(fit_grm_benev, type = "trace",
     main = "CCI - Sexismo Benevolente")
#Aquí se ve más heterogeneidad entre ítems. ESA12 tiene curvas muy juntas y con transiciones muy marcadas. ESA13 tiene un patrón invertido.
#ESA06 tiene curvas casi planas y superpuestas. ESA01, ESA03, ESA17 y ESA22 muestran curvas ordenadas similares a las del sexismo hostil. 
plot(fit_grm_benev, type = "info",
     main = "Información - Sexismo Benevolente")
#La curva muestra tres picos, lo que refleja la heterogeneidad de los ítems. La información se concentra en el rango negativo de θ (entre -2.5 y 0), lo que indica que la escala mide con mayor precisión a personas con niveles bajos a medios de sexismo benevolente. A partir de θ = 1 la información cae marcadamente, lo que significa que la escala tiene poca precisión para discriminar entre personas con niveles altos del rasgo. 
#Comparado con el sexismo hostil, cuya curva era más simétrica, esta escala tiene cobertura más asimétrica del rasgo. 

#8. PLS-SEM MODELO 1 (SEXISMO HOSTIL Y BENEVOLENTE)

#Como el AFC ya validó la estructura factorial, en PLS-SEM se tratan las variables a partir de sus puntajes totales 

#Se define el modelo y qué variables miden qué constructos
medida_m1 <- constructs(
  composite("Impulsividad", single_item("EIP")),
  composite("SexHostil",    single_item("SEXHOS")),
  composite("SexBenev",     single_item("SEXBENE"))
)

#Se define el modelo estructural

#La impulsividad predice el sexismo hostil y el benevolente 
estructura_m1 <- relationships(
  paths(from = "Impulsividad", to = c("SexHostil", "SexBenev"))
)

modelo_m1 <- estimate_pls(
  data = datos_m1,
  measurement_model = medida_m1,
  structural_model = estructura_m1
)

summary(modelo_m1)

#R² indica el porcentaje de varianza del sexismo que explica la impulsividad

#Bootstraping 
boot_m1 <- bootstrap_model(modelo_m1,
                           nboot = 1000,
                           seed = 123)
summary(boot_m1)

#Capacidad predictiva (PLSpredict
pred_m1 <- predict_pls(
  model = modelo_m1,
  technique = predict_DA,
  noFolds = 10,
  reps = 10
)
summary(pred_m1)

#Gráfico modelo 1 
plot(modelo_m1)
plot(boot_m1)

#RESULTADO: la impulsividad predice de manera positiva y estadísticamente significativa el sexismo hostil. Es decir, que a mayor impulsividad, mayor sexismo hostil.  
#sin embargo, el R² de 0.056% indica un tamaño de efecto pequeño en donde la impulsividad solo explica el 5.6% de la varianza del sexismo hostil.

#En el caso del sexismo benevolente no hay relación significativa y el R² es de 0.001. Lo que indica que la impulsividad no predice el sexismo benevolente en la muestra.

#9. PLS-SEM: MODELO 1 CON EDAD COMO COVARIABLE

#La edad se incluye como covariable para controlar su efecto y evaluar si la impulsividad predice
#el sexismo más allá de lo que ya explica la edad

sum(is.na(datos_m1$EDAD)) #5 casos con NA en edad

datos_m1_edad <- datos_m1[complete.cases(datos_m1$EDAD), ]
nrow(datos_m1_edad) #Se eliminan los 5 casos 

#Se añade edad como constructo adicional

medida_m1_cov <- constructs(
  composite("Impulsividad", single_item("EIP")),
  composite("SexHostil",    single_item("SEXHOS")),
  composite("SexBenev",     single_item("SEXBENE")),
  composite("Edad",         single_item("EDAD"))
)

estructura_m1_cov <- relationships(
  paths(from = "Impulsividad", to = c("SexHostil", "SexBenev")),
  paths(from = "Edad",         to = c("SexHostil", "SexBenev"))
)

modelo_m1_cov <- estimate_pls(
  data = datos_m1_edad,
  measurement_model = medida_m1_cov,
  structural_model = estructura_m1_cov
)

summary(modelo_m1_cov)

#Bootstrapping 
boot_m1_cov <- bootstrap_model(modelo_m1_cov,
                               nboot = 1000,
                               seed = 123)
summary(boot_m1_cov)

#PLSpredict
pred_m1_cov <- predict_pls(
  model = modelo_m1_cov,
  technique = predict_DA,
  noFolds = 10,
  reps = 10
)
summary(pred_m1_cov)

#Gráfico modelo 1 con covariable 
plot(modelo_m1_cov)
plot(boot_m1_cov)

#RESULTADOS: la edad no mostró un efecto significativo sobre el sexismo hostil, pero sí predijo de manera significativa y positiva
#que a mayor edad, hay mayor sexismo benevolente. Lo que sugiere que esta dimensión se asocia más con factores sociodemográficos que con rasgos de personalidad como la impulsividad. 

#10. PLS-SEM: MODELO 2 (SUBDIMENSIONES SEXISMO BENEVOLENTE)
#Se evalúa si la impulsividad predice cada subdimensión del sexismo benevolente por separado

medida_m2 <- constructs(
  composite("Impulsividad", single_item("EIP")),
  composite("Paternalismo", single_item("PATERPRO")),
  composite("DifGenero",    single_item("DIGENERO")),
  composite("Intimidad",    single_item("INTIMIDAD"))
)

estructura_m2 <- relationships(
  paths(from = "Impulsividad", to = c("Paternalismo", "DifGenero", "Intimidad"))
)

modelo_m2 <- estimate_pls(
  data = datos_m2,
  measurement_model = medida_m2,
  structural_model = estructura_m2
)

summary(modelo_m2)

#Bootstraping
boot_m2 <- bootstrap_model(modelo_m2,
                           nboot = 1000,
                           seed = 123)
summary(boot_m2)

#PLSpredict

pred_m2 <- predict_pls(
  model = modelo_m2,
  technique = predict_DA,
  noFolds = 10,
  reps = 10
)
summary(pred_m2)

#Gráfico MODELO 2
plot(modelo_m2)
plot(boot_m2)

#RESULTADOS: la impulsividad no predice significativamente ninguna de las tres subdimensiones del sexismo benevolente. Este resultado es consistentecon el modelo 1 y refuerza la idea de que la impulsividad no se relaciona
# con las manifestaciones benevolentes del sexismo, independientemente de cómo se operacionalicen (puntaje total o subdimensiones).

#11. PLS-SEM: MODELO 2 CON EDAD COMO COVARIABLE
#Se incluye la edad para controlar su efecto y evaluar si la impulsividad predice las subdimensiones más allá de lo que ya explica la edad.
datos_m2_edad <- datos_m2[complete.cases(datos_m2$EDAD), ]
nrow(datos_m2_edad) 

medida_m2_cov <- constructs(
  composite("Impulsividad", single_item("EIP")),
  composite("Paternalismo", single_item("PATERPRO")),
  composite("DifGenero",    single_item("DIGENERO")),
  composite("Intimidad",    single_item("INTIMIDAD")),
  composite("Edad",         single_item("EDAD"))
)

estructura_m2_cov <- relationships(
  paths(from = "Impulsividad", to = c("Paternalismo","DifGenero","Intimidad")),
  paths(from = "Edad",         to = c("Paternalismo","DifGenero","Intimidad"))
)

modelo_m2_cov <- estimate_pls(
  data = datos_m2_edad,
  measurement_model = medida_m2_cov,
  structural_model = estructura_m2_cov
)

summary(modelo_m2_cov)

#Bootstraping 
boot_m2_cov <- bootstrap_model(modelo_m2_cov,
                               nboot = 1000,
                               seed = 123)
summary(boot_m2_cov)

#PLSpredict
pred_m2_cov <- predict_pls(
  model = modelo_m2_cov,
  technique = predict_DA,
  noFolds = 10,
  reps = 10
)
summary(pred_m2_cov)

#Gráfico modelo 2 con covariable
plot(modelo_m2_cov)
plot(boot_m2_cov)

#RESULTADOS: En cuanto al efecto de la edad, la única relación positiva significativa es edad y paternalismo protector, en donde 
#a mayor edad tienden a observarse puntuaciones más altas en paternalismo protector. Estos resultados sugieren que las manifestaciones benevolentes del sexismo se relacionan más con factores asociados a la edad que con la impulsividad.

#12. ANÁLISIS MULTIGRUPO
#Para evaluar si la relación predictiva entre impulsividad y sexismo
#varía según el grupo poblacional y según el sexo

#MODELO 1 POR GRUPO (Reclusos vs Estudiantes)
MGA_grupo <- estimate_pls_mga(
  pls_model = modelo_m1,
  condition = datos_m1$GRUPO == "Recluso",
  nboot = 1000,
  seed = 123
)
summary(MGA_grupo)
print(MGA_grupo)

modelo_m1_recluso <- estimate_pls(
  data              = datos_m1[datos_m1$GRUPO == "Recluso", ],
  measurement_model = medida_m1,
  structural_model  = estructura_m1)

modelo_m1_estudiante <- estimate_pls(
  data              = datos_m1[datos_m1$GRUPO == "Estudiante", ],
  measurement_model = medida_m1,
  structural_model  = estructura_m1)

plot(modelo_m1_recluso)
plot(modelo_m1_estudiante)
# Resultado = no hay diferencias significativas entre grupos. Aunque ambos grupos muestran una relación positiva entre impulsividad y sexismo hostil, siendo más 
#pronunciada en reclusos. 

#En el caso del benevolente independientemente de si la persona es reclusa o estudiante la impulsividad no predice el sexismo benevolente

# MODELO 1 POR SEXO 

datos_m1_sexo <- datos_m1[!is.na(datos_m1$SEXO), ]
nrow(datos_m1_sexo)
modelo_m1_sexo <- estimate_pls(
  data = datos_m1_sexo,
  measurement_model = medida_m1,
  structural_model = estructura_m1
)
MGA_sexo <- estimate_pls_mga(
  pls_model = modelo_m1_sexo,
  condition = datos_m1_sexo$SEXO == "Mujer",
  nboot = 1000,
  seed = 123
)
summary(MGA_sexo)
print(MGA_sexo)

modelo_m1_mujer <- estimate_pls(
  data              = datos_m1_sexo[datos_m1_sexo$SEXO == "Mujer", ],
  measurement_model = medida_m1,
  structural_model  = estructura_m1)

modelo_m1_hombre <- estimate_pls(
  data              = datos_m1_sexo[datos_m1_sexo$SEXO == "Hombre", ],
  measurement_model = medida_m1,
  structural_model  = estructura_m1)

plot(modelo_m1_mujer)
plot(modelo_m1_hombre)
#Resultado = ninguna diferencia significativa entre hombres y mujeres. Ambos sexos muestran una relación positiva similar entre impulsividad y sexismo hostil. 
#En cuanto al sexismo benevolente, en mujeres la relación es levemente positiva, mientras que en hombres es negativa. Lo que sugiere que a mayor impulsividad, los hombres tienden a puntuar más bajo en sexismo benevolente.
#Aunque la diferencia no alcanza significación estadística (p valor de 0.065), la dirección opuesta es teóricamente relevante y podría explorarse en muestras más grandes. 

#MODELO 2 POR GRUPO 
MGA_grupo_m2 <- estimate_pls_mga(
  pls_model = modelo_m2,
  condition = datos_m2$GRUPO == "Recluso",
  nboot = 1000,
  seed = 123
)
summary(MGA_grupo_m2)
print(MGA_grupo_m2)

modelo_m2_recluso <- estimate_pls(
  data              = datos_m2[datos_m2$GRUPO == "Recluso", ],
  measurement_model = medida_m2,
  structural_model  = estructura_m2)

modelo_m2_estudiante <- estimate_pls(
  data              = datos_m2[datos_m2$GRUPO == "Estudiante", ],
  measurement_model = medida_m2,
  structural_model  = estructura_m2)

plot(modelo_m2_recluso)
plot(modelo_m2_estudiante)
#RESULTADOS = ninguna diferencia significativa entre reclusos y estudiantes en las tres subdimensiones. Coeficientes cercanos a 0 o R² nulos.

# MODELO 2 POR SEXO
datos_m2_sexo <- datos_m2[!is.na(datos_m2$SEXO), ]

modelo_m2_sexo <- estimate_pls(
  data = datos_m2_sexo,
  measurement_model = medida_m2,
  structural_model = estructura_m2
)

MGA_sexo_m2 <- estimate_pls_mga(
  pls_model = modelo_m2_sexo,
  condition = datos_m2_sexo$SEXO == "Mujer",
  nboot = 1000,
  seed = 123
)
summary(MGA_sexo_m2 )
print(MGA_sexo_m2)

modelo_m2_mujer <- estimate_pls(
  data              = datos_m2_sexo[datos_m2_sexo$SEXO == "Mujer", ],
  measurement_model = medida_m2,
  structural_model  = estructura_m2)

modelo_m2_hombre <- estimate_pls(
  data              = datos_m2_sexo[datos_m2_sexo$SEXO == "Hombre", ],
  measurement_model = medida_m2,
  structural_model  = estructura_m2)

plot(modelo_m2_mujer)
plot(modelo_m2_hombre)
# RESULTADOS: ninguna diferencia significativa entre mujeres y hombres.
#Aún así, hay un patron en las tres subdimensiones del sexismo benevolente: los hombres muestran coeficientes negativos mientras que en 
#las mujeres son positivos o cercanos a 0. Esto replica la tendencia observada en el modelo 1 por sexo (impulsividad → SexBenev) y sugiere que la relación entre impulsividad y sexismo benevolente podría tener una dirección
#diferente según el sexo, lo cual merece exploración en muestras más grandes con mayor potencia estadística.
