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

#Gráficos
plot(fit_grm_eip, type = "trace",
     main = "Curvas Características de los Ítems - EIP")

plot(fit_grm_eip, type = "infotrace",
     main = "Información por Ítem - EIP")

plot(fit_grm_eip, type = "info",
     main = "Información Total del Test - EIP")

plot(fit_grm_eip, type = "SE",
     main = "Error Estándar - EIP")

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

#Gráficos

plot(fit_grm_hostil, type = "trace",
     main = "CCI - Sexismo Hostil")

plot(fit_grm_hostil, type = "info",
     main = "Información - Sexismo Hostil")

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

#Gráficos

plot(fit_grm_benev, type = "trace",
     main = "CCI - Sexismo Benevolente")

plot(fit_grm_benev, type = "info",
     main = "Información - Sexismo Benevolente")


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

#RESULTADO: la impulsividad predice de manera positiva y estadísticamente significativa el sexismo hostil. 
#sin embargo, solo explica el 5.6% de las diferencias individuales en sexismo hostil, por lo que el tamaño del efecto es pequeño.
#En el caso del sexismo benevolente la impulsividad no predice significativamente, así que no se demuestra una relación entre ambas variables.

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
