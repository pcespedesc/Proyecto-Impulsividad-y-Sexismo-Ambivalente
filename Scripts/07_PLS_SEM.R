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
