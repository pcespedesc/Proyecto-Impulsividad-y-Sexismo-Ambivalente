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

#Resultado: la edad no mostró un efecto significativo sobre el sexismo hostil, pero sí predijo positivamente
#el sexismo benevolente (pasó de explicar el 0.1% al 2.9%)

#10. PLS-SEM: MODELO 2 (SUBDIMENSIONES SEXISMO BENEVOLENTE)

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

#La impuslividad no predijo significativamente ninguna de las subdimensiones del sexismo benevolente

#11. PLS-SEM: MODELO 2 CON EDAD COMO COVARIABLE

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

#RESULTADO: En cuanto al efecto de la edad, la única relación positiva significativa es edad y paternalismo protector, en donde 
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

# Resultado = no hay diferencias significativas entre grupos

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

#Resultado = ninguna diferencia entre hombres y mujeres. Aunque no es estadisticamente significativo, se observa que en hombres la relación entre impulsividad y sexismo benevolente es negativa, lo que podría explorarse en muestras más grandes

#MODELO 2 POR GRUPO 
MGA_grupo_m2 <- estimate_pls_mga(
  pls_model = modelo_m2,
  condition = datos_m2$GRUPO == "Recluso",
  nboot = 1000,
  seed = 123
)
summary(MGA_grupo_m2)
print(MGA_grupo_m2)

#Resultado = ninguna diferencia significativa entre reclusos y estudiantes

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

# Resultado: ninguna diferencia significativa entre mujeres y hombres

